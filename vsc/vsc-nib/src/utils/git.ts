import * as vscode from 'vscode';
import * as cp from 'child_process';
import * as path from 'path';
import { promisify } from 'util';

const execAsync = promisify(cp.exec);

export async function getCurrentBranch(): Promise<string | undefined> {
  try {
    // Method 1: Use VSCode's built-in Git extension API
    const gitExtension = vscode.extensions.getExtension('vscode.git');
    if (gitExtension) {
      const git = gitExtension.exports.getAPI(1);
      if (git.repositories.length > 0) {
        const repo = git.repositories[0];
        return repo.state.HEAD?.name;
      }
    }

    // Method 2: Fallback to command-line git
    const workspaceFolder = vscode.workspace.workspaceFolders?.[0];
    if (workspaceFolder) {
      const { stdout } = await execAsync('git branch --show-current', {
        cwd: workspaceFolder.uri.fsPath
      });
      return stdout.trim();
    }
  } catch (error) {
    console.error('Error getting current branch:', error);
  }

  return undefined;
}

export async function cloneRepository(
  repoUrl: string,
  targetPath: string,
  onProgress?: (data: string) => void
): Promise<void> {
  return new Promise((resolve, reject) => {
    const process = cp.spawn('git', ['clone', repoUrl, targetPath], {
      cwd: path.dirname(targetPath)
    });

    process.stdout.on('data', (data) => {
      if (onProgress) {
        onProgress(data.toString());
      }
    });

    process.stderr.on('data', (data) => {
      if (onProgress) {
        onProgress(data.toString());
      }
    });

    process.on('close', (code) => {
      if (code === 0) {
        resolve();
      } else {
        reject(new Error(`Git clone failed with code ${code}`));
      }
    });

    process.on('error', (error) => {
      reject(error);
    });
  });
}
