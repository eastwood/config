import * as vscode from 'vscode';
import * as path from 'path';
import * as fs from 'fs/promises';
import { getConfig } from '../utils/config';
import { cloneRepository } from '../utils/git';

export async function cloneRepo(): Promise<void> {
  try {
    const config = getConfig();

    // Prompt for repository name
    const repoName = await vscode.window.showInputBox({
      prompt: 'Enter repository name',
      placeHolder: 'my-repo'
    });

    if (!repoName) {
      return; // User cancelled
    }

    // Determine clone directory
    let cloneDir = config.cloneDirectory;

    if (!cloneDir) {
      const selectedUri = await vscode.window.showOpenDialog({
        canSelectFiles: false,
        canSelectFolders: true,
        canSelectMany: false,
        title: 'Select directory to clone repository into'
      });

      if (!selectedUri || selectedUri.length === 0) {
        return; // User cancelled
      }

      cloneDir = selectedUri[0].fsPath;
    }

    // Ensure directory exists
    await fs.mkdir(cloneDir, { recursive: true });

    // Construct repository URL
    const repoUrl = `git@github.com:${config.githubOrganization}/${repoName}.git`;
    const targetPath = path.join(cloneDir, repoName);

    // Check if directory already exists
    try {
      await fs.access(targetPath);
      vscode.window.showWarningMessage(`Directory ${targetPath} already exists`);
      return;
    } catch {
      // Directory doesn't exist, proceed with cloning
    }

    // Show progress
    await vscode.window.withProgress({
      location: vscode.ProgressLocation.Notification,
      title: `Cloning ${repoName}`,
      cancellable: false
    }, async (progress) => {
      progress.report({ message: 'Cloning repository...' });

      await cloneRepository(repoUrl, targetPath, (data) => {
        progress.report({ message: data });
      });
    });

    // Open in new window
    const openChoice = await vscode.window.showInformationMessage(
      `Repository cloned to ${targetPath}`,
      'Open in New Window',
      'Open in Current Window',
      'Don\'t Open'
    );

    if (openChoice === 'Open in New Window') {
      await vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(targetPath), true);
    } else if (openChoice === 'Open in Current Window') {
      await vscode.commands.executeCommand('vscode.openFolder', vscode.Uri.file(targetPath), false);
    }
  } catch (error) {
    vscode.window.showErrorMessage(`Clone failed: ${error}`);
  }
}
