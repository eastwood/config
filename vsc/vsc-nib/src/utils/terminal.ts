import * as vscode from 'vscode';

export function createInteractiveTerminal(
  name: string,
  command: string,
  env?: { [key: string]: string }
): vscode.Terminal {
  // Create a new terminal with custom environment variables
  const terminal = vscode.window.createTerminal({
    name,
    env: env ? { ...process.env, ...env } : undefined
  });

  // Show the terminal
  terminal.show(true);

  // Send the command
  terminal.sendText(command);

  return terminal;
}
