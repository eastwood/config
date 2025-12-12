import * as vscode from 'vscode';
import { getConfig } from '../utils/config';

export async function createReviewNotes(): Promise<void> {
  try {
    const config = getConfig();

    // Collect inputs
    const jiraId = await vscode.window.showInputBox({
      prompt: 'Enter Jira ID',
      placeHolder: 'PHISL-1000'
    });

    if (!jiraId) {
      return;
    }

    const pullRequest = await vscode.window.showInputBox({
      prompt: 'Enter Pull Request URL',
      placeHolder: 'https://github.com/org/repo/pull/123'
    });

    if (!pullRequest) {
      return;
    }

    const buildkite = await vscode.window.showInputBox({
      prompt: 'Enter Buildkite URL',
      placeHolder: 'https://buildkite.com/org/project/builds/123'
    });

    if (!buildkite) {
      return;
    }

    // Generate review notes template
    const reviewNotesTemplate = generateReviewNotesTemplate(jiraId, pullRequest, buildkite, config.jiraBaseUrl);

    // Create a new document with the template
    const doc = await vscode.workspace.openTextDocument({
      content: reviewNotesTemplate,
      language: 'markdown'
    });

    // Open in the first column
    const editor = await vscode.window.showTextDocument(doc, { viewColumn: vscode.ViewColumn.One });

    // Position cursor at the notes section (after "**Notes:**\n\n")
    const notesPosition = doc.getText().indexOf('**Notes:**\n\n') + '**Notes:**\n\n'.length;
    const position = doc.positionAt(notesPosition);
    editor.selection = new vscode.Selection(position, position);
    editor.revealRange(new vscode.Range(position, position));

    // Explicitly focus back on the editor in column one
    await vscode.window.showTextDocument(doc, {
      viewColumn: vscode.ViewColumn.One,
      preserveFocus: false,
      preview: false
    });


    vscode.window.showInformationMessage('Review notes template created. Fill in your notes, then copy when ready.');
  } catch (error) {
    vscode.window.showErrorMessage(`Failed to create review notes: ${error}`);
  }
}

function generateReviewNotesTemplate(jiraId: string, pullRequest: string, buildkite: string, jiraBaseUrl: string): string {
  return `👋 **[${jiraId}](${jiraBaseUrl}/browse/${jiraId})** is ready for review 🙏

**Pull Request:** ${pullRequest}

**Buildkite:** ${buildkite}

**Notes:**

- 
`;
}
