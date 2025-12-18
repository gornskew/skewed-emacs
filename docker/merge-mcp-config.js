#!/usr/bin/env node

// MCP config setup for skewed-emacs container
// For now, just copies the base config to /tmp
// TODO: Add private services overlay protocol

const fs = require('fs');
const path = require('path');
const os = require('os');

const homeDir = os.homedir();
const baseConfigPath = path.join(homeDir, 'skewed-emacs/docker/mcp-config.json');
const outputPath = '/tmp/merged-mcp-config.json';

try {
    const baseConfig = JSON.parse(fs.readFileSync(baseConfigPath, 'utf8'));
    
    fs.writeFileSync(outputPath, JSON.stringify(baseConfig, null, 4));
    
    console.log('MCP config ready at:', outputPath);
    console.log('Available servers:', Object.keys(baseConfig.mcpServers).join(', '));
    
} catch (error) {
    console.error('Error preparing MCP config:', error.message);
    process.exit(1);
}
