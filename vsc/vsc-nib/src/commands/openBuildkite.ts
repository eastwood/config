import * as vscode from 'vscode';
import * as path from 'path';
import { getConfig } from '../utils/config';

export async function openBuildkite(): Promise<void> {
  try {
    const config = getConfig();

    if (!config.buildkiteBaseUrl || !config.buildkiteOrganization) {
      vscode.window.showErrorMessage('Buildkite configuration is incomplete. Please update settings.');
      return;
    }

    // Get current workspace folder name as default
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    const defaultProject = workspaceFolder
      ? path.basename(workspaceFolder.uri.fsPath)
      : '';

    // Prompt for project name
    const projectName = await vscode.window.showInputBox({
      prompt: 'Enter Buildkite project name',
      value: defaultProject,
      placeHolder: 'my-project'
    });

    if (!projectName) {
      return; // User cancelled
    }

    // Construct Buildkite URL
    const buildkiteUrl = `${config.buildkiteBaseUrl}/${config.buildkiteOrganization}/${projectName}`;

    // Open URL in default browser
    await vscode.env.openExternal(vscode.Uri.parse(buildkiteUrl));

    vscode.window.showInformationMessage(`Opening Buildkite project: ${projectName}`);
  } catch (error) {
    vscode.window.showErrorMessage(`Failed to open Buildkite: ${error}`);
  }
}
