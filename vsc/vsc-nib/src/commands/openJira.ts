import * as vscode from 'vscode';
import { getConfig } from '../utils/config';
import { getCurrentBranch } from '../utils/git';

export async function openJira(): Promise<void> {
  try {
    const config = getConfig();

    if (!config.jiraBaseUrl) {
      vscode.window.showErrorMessage('Jira base URL is not configured. Please update settings.');
      return;
    }

    // Get current branch as default
    const currentBranch = await getCurrentBranch();

    // Prompt for Jira ticket ID
    const ticketId = await vscode.window.showInputBox({
      prompt: 'Enter Jira ticket ID',
      value: currentBranch || '',
      placeHolder: 'PHISL-1000'
    });

    if (!ticketId) {
      return; // User cancelled
    }

    // Construct Jira URL
    const jiraUrl = `${config.jiraBaseUrl}/browse/${ticketId}`;

    // Open URL in default browser
    await vscode.env.openExternal(vscode.Uri.parse(jiraUrl));

    vscode.window.showInformationMessage(`Opening Jira ticket: ${ticketId}`);
  } catch (error) {
    vscode.window.showErrorMessage(`Failed to open Jira: ${error}`);
  }
}
