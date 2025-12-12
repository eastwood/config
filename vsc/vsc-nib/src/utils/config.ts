import * as vscode from 'vscode';

export interface NibConfig {
  cloneDirectory: string;
  githubOrganization: string;
  salesZoomUrl: string;
  jiraBaseUrl: string;
  buildkiteBaseUrl: string;
  buildkiteOrganization: string;
  rqpDefaultStage: string;
}

export function getConfig(): NibConfig {
  const config = vscode.workspace.getConfiguration('nib');

  return {
    cloneDirectory: config.get<string>('cloneDirectory', ''),
    githubOrganization: config.get<string>('githubOrganization', 'nib-group'),
    salesZoomUrl: config.get<string>('salesZoomUrl', ''),
    jiraBaseUrl: config.get<string>('jiraBaseUrl', 'https://nibgroup.atlassian.net'),
    buildkiteBaseUrl: config.get<string>('buildkiteBaseUrl', 'https://buildkite.com'),
    buildkiteOrganization: config.get<string>('buildkiteOrganization', 'nib-health-funds-ltd'),
    rqpDefaultStage: config.get<string>('rqpDefaultStage', 'kaos')
  };
}
