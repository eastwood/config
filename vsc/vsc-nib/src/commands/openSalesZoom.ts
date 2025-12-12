import * as vscode from 'vscode';
import { getConfig } from '../utils/config';

export async function openSalesZoom(): Promise<void> {
  try {
    const config = getConfig();

    if (!config.salesZoomUrl) {
      vscode.window.showErrorMessage('Sales Zoom URL is not configured. Please update settings.');
      return;
    }

    // Open URL in default browser
    await vscode.env.openExternal(vscode.Uri.parse(config.salesZoomUrl));

    vscode.window.showInformationMessage('Opening Sales Zoom meeting');
  } catch (error) {
    vscode.window.showErrorMessage(`Failed to open Zoom: ${error}`);
  }
}
