import * as vscode from 'vscode';
import { getConfig } from '../utils/config';
import { createInteractiveTerminal } from '../utils/terminal';

export async function rqpAuth(): Promise<void> {
  try {
    const config = getConfig();

    // Prompt for stage with default value
    const stage = await vscode.window.showInputBox({
      prompt: 'Enter stage for RQP authentication',
      value: config.rqpDefaultStage,
      placeHolder: 'kaos'
    });

    if (!stage) {
      return; // User cancelled
    }

    // Construct the command
    const command = `rqp auth --sso -s ${stage}`;

    // Create interactive terminal
    createInteractiveTerminal('RQP Auth', command);

    vscode.window.showInformationMessage(`Starting RQP authentication for stage: ${stage}`);
  } catch (error) {
    vscode.window.showErrorMessage(`RQP Auth failed: ${error}`);
  }
}
