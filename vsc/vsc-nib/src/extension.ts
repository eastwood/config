import * as vscode from 'vscode';
import * as commands from './commands';

export function activate(context: vscode.ExtensionContext) {
  console.log('Nib Utilities extension is now active');

  // Register all commands
  const commandRegistrations = [
    vscode.commands.registerCommand('nib.rqpAuth', commands.rqpAuth),
    vscode.commands.registerCommand('nib.cloneRepo', commands.cloneRepo),
    vscode.commands.registerCommand('nib.openSalesZoom', commands.openSalesZoom),
    vscode.commands.registerCommand('nib.openJira', commands.openJira),
    vscode.commands.registerCommand('nib.openBuildkite', commands.openBuildkite),
    vscode.commands.registerCommand('nib.createReviewNotes', commands.createReviewNotes)
  ];

  // Add all commands to subscriptions so they are disposed when extension is deactivated
  commandRegistrations.forEach(cmd => context.subscriptions.push(cmd));
}

export function deactivate() {
  console.log('Nib Utilities extension is now deactivated');
}
