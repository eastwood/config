# Nib Utilities

A VSCode extension that provides utility commands for Nib development workflows.

## Features

### Commands

All commands are accessible via the Command Palette (`Ctrl+Shift+P` / `Cmd+Shift+P`):

#### Nib: RQP Auth (SSO)
Authenticate with RQP using SSO. Opens an interactive terminal and runs the authentication command.
- Prompts for stage (defaults to "kaos")
- Runs: `rqp auth --sso -s {stage}`

#### Nib: Clone Repository
Clone a repository from your GitHub organization.
- Prompts for repository name
- Clones from configured GitHub organization (default: nib-group)
- Option to open cloned repository in a new or current window

#### Nib: Open Sales Zoom
Opens the configured Sales Zoom meeting in your default browser.

#### Nib: Open Jira Ticket
Opens a Jira ticket in your default browser.
- Prompts for ticket ID
- Defaults to current git branch name
- Opens: `{jiraBaseUrl}/browse/{ticketId}`

#### Nib: Open Buildkite
Opens a Buildkite project in your default browser.
- Prompts for project name
- Defaults to current workspace folder name
- Opens: `{buildkiteBaseUrl}/{org}/{project}`

#### Nib: Create Review Notes
Generates a review notes template.
- Prompts for: Jira ID, Pull Request URL, Buildkite URL
- Opens a markdown template with live preview side-by-side
- Cursor automatically positioned at the notes section
- Fill in your notes and see them render in real-time
- Copy from the preview for HTML, or from the markdown for Jira
- **For Jira:** Copy the markdown directly - Jira supports markdown formatting
- **For HTML:** Copy from the rendered preview panel

## Configuration

All settings can be configured in VSCode settings (`Ctrl+,` / `Cmd+,`):

### `nib.cloneDirectory`
Directory where repositories will be cloned. Leave empty to be prompted each time.

**Default:** `""`

### `nib.githubOrganization`
GitHub organization name for cloning repositories.

**Default:** `"nib-group"`

### `nib.salesZoomUrl`
Zoom meeting URL for sales calls.

**Default:** `"https://nibgroup.zoom.us/j/815911628?pwd=UnJBQ2hYaFBxOHBJazNzdzJ6TDc2UT09"`

### `nib.jiraBaseUrl`
Base URL for your Jira instance.

**Default:** `"https://nibgroup.atlassian.net"`

### `nib.buildkiteBaseUrl`
Base URL for Buildkite.

**Default:** `"https://buildkite.com"`

### `nib.buildkiteOrganization`
Buildkite organization name.

**Default:** `"nib-health-funds-ltd"`

### `nib.rqpDefaultStage`
Default stage for RQP authentication.

**Default:** `"kaos"`

## Installation

### From VSIX
1. Package the extension: `npm run package`
2. Install: `code --install-extension vsc-nib-0.1.0.vsix`

### From Source
1. Clone this repository
2. Run `npm install`
3. Press `F5` to open a new VSCode window with the extension loaded

## Development

### Building
```bash
npm install
npm run compile
```

### Watch Mode
```bash
npm run watch
```

### Debugging
Press `F5` in VSCode to launch the Extension Development Host.

## Requirements

- VSCode 1.85.0 or higher
- Git (for repository operations)
- RQP CLI (for authentication command)

## License

MIT
