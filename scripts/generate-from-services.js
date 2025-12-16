#!/usr/bin/env node
/**
 * generate-from-services.js
 * 
 * Reads services.json (Single Source of Truth) and generates:
 *   - docker-compose.yml
 *   - mcp-config.json
 *   - services-discovery.el (for Emacs dashboard)
 *   - Optionally SSH keys if not present
 */

const fs = require('fs');
const path = require('path');
const yaml = require('yaml');  // npm install yaml

const SCRIPT_DIR = path.dirname(__filename);
const ROOT_DIR = path.dirname(SCRIPT_DIR);

// Parse arguments
const withPrivate = process.argv.includes('--with-private');
const generateSshKeys = process.argv.includes('--generate-ssh-keys');

// Load services.json
function loadServices() {
    const servicesPath = path.join(ROOT_DIR, 'services.json');
    if (!fs.existsSync(servicesPath)) {
        console.error('Error: services.json not found at', servicesPath);
        process.exit(1);
    }
    
    let services = JSON.parse(fs.readFileSync(servicesPath, 'utf8'));
    
    // Merge private overlay if requested
    if (withPrivate) {
        const privatePath = path.join(ROOT_DIR, 'services-private.json');
        if (fs.existsSync(privatePath)) {
            const privateServices = JSON.parse(fs.readFileSync(privatePath, 'utf8'));
            services.services = { ...services.services, ...privateServices.services };
            console.log('Merged services-private.json');
        } else {
            console.warn('Warning: --with-private specified but services-private.json not found');
        }
    }
    
    return services;
}

// Generate docker-compose.yml
function generateDockerCompose(config) {
    const compose = {
        networks: {
            'skewed-network': {
                name: '${DOCKER_NETWORK_NAME:-skewed-network}',
                driver: 'bridge'
            }
        },
        services: {}
    };
    
    const defaults = config.defaults || {};
    
    for (const [name, svc] of Object.entries(config.services)) {
        const service = {
            image: svc.image,
            container_name: name,
            hostname: name,
            restart: defaults.restart || 'unless-stopped',
            stdin_open: true,
            tty: true,
            ports: [],
            environment: [],
            volumes: [],
            networks: ['skewed-network']
        };
        
        // Add ports
        if (svc.ports) {
            for (const [portType, portConfig] of Object.entries(svc.ports)) {
                service.ports.push(`${portConfig.host}:${portConfig.container}`);
            }
        }
        
        // Add environment variables for ports (so containers know their own config)
        if (svc.ports?.http) {
            service.environment.push(`HTTP_PORT=${svc.ports.http.container}`);
            service.environment.push(`HTTP_HOST_PORT=${svc.ports.http.host}`);
        }
        if (svc.ports?.swank) {
            service.environment.push(`SWANK_PORT=${svc.ports.swank.container}`);
            service.environment.push(`SWANK_HOST_PORT=${svc.ports.swank.host}`);
            service.environment.push('START_SWANK=true');
        }
        service.environment.push('START_HTTP=true');
        service.environment.push(`TZ=${defaults.tz || '${TZ:-Etc/UTC}'}`);
        
        // Add default volumes
        service.volumes = [
            {
                type: 'bind',
                source: '${PROJECTS_DIR}',
                target: '/projects',
                bind: { create_host_path: true }
            }
        ];
        
        // Add healthcheck
        if (svc.healthcheck) {
            service.healthcheck = {
                test: ['CMD', 'curl', '-f', `http://localhost:${svc.ports?.http?.container || 80}${svc.healthcheck.endpoint}`],
                interval: svc.healthcheck.interval || '60s',
                timeout: '3s',
                retries: 3,
                start_period: '15s'
            };
        }
        
        // Special handling for certain service types
        if (svc.type === 'common-lisp') {
            service.user = 'root';
            service.extra_hosts = ['host.docker.internal:host-gateway'];
            service.group_add = ['${DOCKER_GROUP_ID:-999}'];
        }
        
        if (svc.type === 'middleware' && svc.ssh?.enabled) {
            // SSH-enabled middleware gets authorized_keys mount
            service.volumes.push({
                type: 'bind',
                source: '${PROJECTS_DIR}/skewed-emacs/ssh-keys/authorized_keys',
                target: `/home/${svc.ssh.user}/.ssh/authorized_keys`,
                read_only: true
            });
        }
        
        compose.services[name] = service;
    }
    
    return compose;
}

// Generate mcp-config.json
function generateMcpConfig(config) {
    const mcpConfig = { mcpServers: {} };
    
    for (const [name, svc] of Object.entries(config.services)) {
        // Skip services marked as exclude_from_mcp
        if (svc.exclude_from_mcp) continue;
        
        // Only include lisply backends (emacs-lisp or common-lisp)
        if (!['emacs-lisp', 'common-lisp'].includes(svc.type)) continue;
        
        const args = [
            '/app/scripts/mcp-wrapper.js',
            '--server-name', name,
            '--http-host-port', String(svc.ports.http.host),
            '--http-port', String(svc.ports.http.container),
            '--backend-host', name,
            '--no-auto-start'
        ];
        
        if (svc.ports?.swank) {
            args.push('--swank-host-port', String(svc.ports.swank.host));
            args.push('--swank-port', String(svc.ports.swank.container));
        }
        
        mcpConfig.mcpServers[name] = {
            command: 'node',
            args: args
        };
    }
    
    return mcpConfig;
}

// Generate Emacs Lisp discovery file
function generateEmacsDiscovery(config) {
    let elisp = `;;; services-discovery.el --- Auto-generated from services.json -*- lexical-binding: t; -*-
;; DO NOT EDIT - Generated by generate-from-services.js

(defvar skewed-services-config
  '(`;
    
    for (const [name, svc] of Object.entries(config.services)) {
        if (!svc.ports?.http) continue;
        
        elisp += `
    (:name "${name}"
     :type "${svc.type}"
     :lisp-impl "${svc.lisp_impl || (svc.type === 'emacs-lisp' ? 'Emacs' : 'Unknown')}"
     :http-host "${name}"
     :http-port ${svc.ports.http.container}
     :http-host-port ${svc.ports.http.host}`;
        
        if (svc.ports?.swank) {
            elisp += `
     :swank-host "${name}"
     :swank-port ${svc.ports.swank.container}
     :swank-host-port ${svc.ports.swank.host}`;
        }
        
        elisp += `)`;
    }
    
    elisp += `)
  "Services configuration loaded from services.json.")

(defun skewed-get-services ()
  "Return the list of configured services."
  skewed-services-config)

(defun skewed-get-lisply-backends ()
  "Return services suitable for lisply backend display."
  (seq-filter (lambda (svc)
                (member (plist-get svc :type) '("emacs-lisp" "common-lisp")))
              skewed-services-config))

(defun skewed-get-swank-services ()
  "Return services with SWANK ports."
  (seq-filter (lambda (svc) (plist-get svc :swank-port))
              skewed-services-config))

(provide 'services-discovery)
;;; services-discovery.el ends here
`;
    
    return elisp;
}

// Main
function main() {
    console.log('Loading services configuration...');
    const config = loadServices();
    
    // Generate docker-compose.yml
    console.log('Generating docker-compose.yml...');
    const compose = generateDockerCompose(config);
    // Note: We'd need the yaml package for proper YAML output
    // For now, output as JSON that can be converted
    fs.writeFileSync(
        path.join(ROOT_DIR, 'docker-compose.generated.json'),
        JSON.stringify(compose, null, 2)
    );
    console.log('  Written: docker-compose.generated.json');
    
    // Generate mcp-config.json
    console.log('Generating mcp-config.json...');
    const mcpConfig = generateMcpConfig(config);
    fs.writeFileSync(
        path.join(ROOT_DIR, 'mcp-config.generated.json'),
        JSON.stringify(mcpConfig, null, 2)
    );
    console.log('  Written: mcp-config.generated.json');
    
    // Generate Emacs discovery file
    console.log('Generating services-discovery.el...');
    const elisp = generateEmacsDiscovery(config);
    const elispPath = path.join(ROOT_DIR, 'dot-files/emacs.d/etc/services-discovery.el');
    fs.writeFileSync(elispPath, elisp);
    console.log('  Written:', elispPath);
    
    console.log('\nDone! Generated files from services.json');
    if (withPrivate) {
        console.log('(included private services overlay)');
    }
}

main();
