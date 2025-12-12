#!/bin/bash
# Note: We don't use 'set -e' because some AWS API calls may fail gracefully
# (e.g., inference profile creation due to SCP restrictions)

# Parse command line arguments
VERBOSE=0
while getopts "v" opt; do
    case $opt in
        v)
            VERBOSE=1
            ;;
        \?)
            echo "Usage: $0 [-v]"
            echo "  -v  Enable verbose mode (show detailed error output)"
            exit 1
            ;;
    esac
done

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

# Function to print verbose output
log_verbose() {
    if [ $VERBOSE -eq 1 ]; then
        echo -e "${YELLOW}[VERBOSE]${NC} $*"
    fi
}

# Function to run command with optional verbose output
run_cmd() {
    local cmd="$*"
    log_verbose "Running: $cmd"
    
    if [ $VERBOSE -eq 1 ]; then
        eval "$cmd"
        local exit_code=$?
        log_verbose "Exit code: $exit_code"
        return $exit_code
    else
        eval "$cmd" 2>/dev/null
        return $?
    fi
}

# Show verbose mode banner if enabled
if [ $VERBOSE -eq 1 ]; then
    echo ""
    echo -e "${YELLOW}+------------------------------------------------------------------------------+${NC}"
    echo -e "${YELLOW}¦                         VERBOSE MODE ENABLED                                 ¦${NC}"
    echo -e "${YELLOW}¦  Detailed error output and debugging information will be shown              ¦${NC}"
    echo -e "${YELLOW}+------------------------------------------------------------------------------+${NC}"
    echo ""
    sleep 1
fi

# Animated nib logo with color cycling
for color in "${BLUE}" "${PURPLE}" "${GREEN}" "${YELLOW}"; do
    clear 2>/dev/null || true
    echo -e "${color}"
    cat << 'EOF'
                ######                                      ####### #######
          ##    ######    ##                                ####### #######
        #####   ######  ######                              ####### #######
       ######## ###### ########                                     #######
        ######## ####  #######        ###### ##########     ####### ##################
          ######       #####          ###################   ####### ####################
     #################     #######    ####################  ####### ######################
     #################   #########    #######      #######  ####### #########      ########
     #################   #########    ######        ######  ####### #######         #######
     #################     ######     ######        ######  ####### #######          ######
          ############ #####          ######        ######  ####### #######          ######
        ############## #######        ######        ######  ####### ########        #######
       ######## ###### ########       ######        ######  ####### ##########    #########
        #####   ######   #####        ######        ######  ####### ######################
          #     ######     #          ######        ######  ####### ####################
                ######                ######        ######  ######  #################
EOF
    echo -e "${NC}"
    sleep 0.15
done
echo ""
echo -e "${PURPLE}+------------------------------------------------------------------------------+${NC}"
echo -e "${PURPLE}¦                     ${YELLOW}Welcome to the DPX nib Hackathon!${PURPLE}                        ¦${NC}"
echo -e "${PURPLE}¦------------------------------------------------------------------------------¦${NC}"
echo -e "${PURPLE}¦                                                                              ¦${NC}"
echo -e "${PURPLE}¦  This lil script will help you get all setup with Claude Code running on    ¦${NC}"
echo -e "${PURPLE}¦  AWS Bedrock. All the best on the day and as the Hunger Games says,         ¦${NC}"
echo -e "${PURPLE}¦  ${GREEN}may the odds be ever in your favour!${PURPLE}                                       ¦${NC}"
echo -e "${PURPLE}¦                                                                              ¦${NC}"
echo -e "${PURPLE}¦------------------------------------------------------------------------------¦${NC}"
echo -e "${PURPLE}¦              ${BLUE}Claude Code + AWS Bedrock Setup Wizard${PURPLE}                       ¦${NC}"
echo -e "${PURPLE}+------------------------------------------------------------------------------+${NC}"
echo ""

# Function to extract username from RQP credentials
get_username_from_rqp() {
    if [ -f ~/.aws/credentials ]; then
        # Try to extract from assumed role in credentials
        local role_info=$(grep "# Role:" ~/.aws/credentials 2>/dev/null || echo "")
        if [ -n "$role_info" ]; then
            echo "$role_info" | grep -o '[a-zA-Z0-9._%+-]\+@[a-zA-Z0-9.-]\+\.[a-zA-Z]\{2,\}' || echo ""
        else
            # Try to get from AWS STS
            local caller_identity=$(aws sts get-caller-identity --query 'Arn' --output text 2>/dev/null || echo "")
            if [[ "$caller_identity" =~ ([a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}) ]]; then
                echo "${BASH_REMATCH[1]}"
            else
                echo ""
            fi
        fi
    fi
    echo ""
}

# Function to get AWS account ID
get_aws_account_id() {
    aws sts get-caller-identity --query 'Account' --output text 2>/dev/null || echo ""
}

# Function to check if inference profile exists
check_inference_profile_exists() {
    local profile_name="$1"
    aws bedrock list-inference-profiles --region us-west-2 --output json 2>/dev/null | \
        python3 -c "import sys, json; data = json.load(sys.stdin); profiles = [p for p in data.get('inferenceProfileSummaries', []) if p.get('inferenceProfileName') == '$profile_name']; print('yes' if profiles else 'no')"
}

# Function to get existing inference profile ARN
get_inference_profile_arn() {
    local profile_name="$1"
    aws bedrock list-inference-profiles --region us-west-2 --output json 2>/dev/null | \
        python3 -c "import sys, json; data = json.load(sys.stdin); profiles = [p for p in data.get('inferenceProfileSummaries', []) if p.get('inferenceProfileName') == '$profile_name']; print(profiles[0]['inferenceProfileArn'] if profiles else '')"
}

echo -e "${BLUE}Step 1: Checking prerequisites...${NC}"
echo ""

# Check Node.js
log_verbose "Checking for Node.js installation..."
if ! command -v node &> /dev/null; then
    echo -e "${RED}? Node.js not found${NC}"
    echo "  Please install Node.js first:"
    echo "  $ brew install node"
    exit 1
else
    NODE_VERSION=$(node --version 2>&1)
    echo -e "${GREEN}? Node.js installed${NC}"
    log_verbose "Node.js version: $NODE_VERSION"
fi

# Check AWS CLI
log_verbose "Checking for AWS CLI installation..."
if ! command -v aws &> /dev/null; then
    echo -e "${RED}? AWS CLI not found${NC}"
    echo "  Please install AWS CLI first"
    exit 1
else
    AWS_VERSION=$(aws --version 2>&1)
    echo -e "${GREEN}? AWS CLI installed${NC}"
    log_verbose "AWS CLI version: $AWS_VERSION"
fi

# Check RQP credentials
echo ""
echo -e "${BLUE}Step 2: Checking AWS credentials...${NC}"
echo ""

log_verbose "Checking for AWS session token in ~/.aws/credentials..."
if ! grep -q "aws_session_token" ~/.aws/credentials 2>/dev/null; then
    echo -e "${RED}? No AWS session token found${NC}"
    log_verbose "Contents of ~/.aws/credentials:"
    [ $VERBOSE -eq 1 ] && cat ~/.aws/credentials 2>&1 | head -20
    echo ""
    echo "  Please authenticate with RQP first:"
    echo -e "  ${YELLOW}$ rqp auth -r poweruser -s kaos -z secure${NC}"
    echo ""
    exit 1
fi

# Check credential expiry
log_verbose "Checking credential expiry..."
EXPIRY=$(grep "expiration" ~/.aws/credentials | head -1 | awk '{print $3}')
log_verbose "Expiration time found: $EXPIRY"

if [ -n "$EXPIRY" ]; then
    if [ $VERBOSE -eq 1 ]; then
        REMAINING=$(python3 -c "from datetime import datetime, timezone; exp_dt = datetime.fromisoformat('$EXPIRY'.replace('Z', '+00:00')); now_dt = datetime.now(timezone.utc); diff = exp_dt - now_dt; print(int(diff.total_seconds() / 60)) if diff.total_seconds() > 0 else -1" 2>&1)
    else
        REMAINING=$(python3 -c "from datetime import datetime, timezone; exp_dt = datetime.fromisoformat('$EXPIRY'.replace('Z', '+00:00')); now_dt = datetime.now(timezone.utc); diff = exp_dt - now_dt; print(int(diff.total_seconds() / 60)) if diff.total_seconds() > 0 else -1" 2>/dev/null || echo "-1")
    fi
    
    log_verbose "Calculated remaining minutes: $REMAINING"
    
    if [ "$REMAINING" -lt 0 ]; then
        echo -e "${RED}? AWS credentials expired${NC}"
        echo "  Please re-authenticate: rqp auth"
        exit 1
    else
        echo -e "${GREEN}? AWS credentials valid ($REMAINING minutes remaining)${NC}"
    fi
fi

# Get user information
echo ""
echo -e "${BLUE}Step 3: Gathering user information...${NC}"
echo ""

log_verbose "Attempting to extract username from RQP credentials..."
USERNAME=$(get_username_from_rqp)
log_verbose "Extracted username: ${USERNAME:-<none>}"

log_verbose "Fetching AWS account ID..."
if [ $VERBOSE -eq 1 ]; then
    AWS_ACCOUNT=$(aws sts get-caller-identity --query 'Account' --output text 2>&1)
else
    AWS_ACCOUNT=$(get_aws_account_id)
fi
log_verbose "AWS Account ID: ${AWS_ACCOUNT:-<none>}"

if [ -z "$USERNAME" ]; then
    echo -e "${YELLOW}Could not auto-detect email from RQP credentials${NC}"
    echo ""
    read -p "Enter your nib email address: " USERNAME
    
    # Validate email format
    if [[ ! "$USERNAME" =~ ^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$ ]]; then
        echo -e "${RED}? Invalid email format${NC}"
        exit 1
    fi
else
    echo -e "${GREEN}? Detected email: $USERNAME${NC}"
    read -p "Is this correct? (Y/n): " CONFIRM
    if [[ "$CONFIRM" =~ ^[Nn] ]]; then
        read -p "Enter your nib email address: " USERNAME
    fi
fi

if [ -z "$AWS_ACCOUNT" ]; then
    echo -e "${RED}? Could not determine AWS account ID${NC}"
    exit 1
else
    echo -e "${GREEN}? AWS Account: $AWS_ACCOUNT${NC}"
fi

# Create sanitized profile name (remove @ and . for AWS naming rules)
# Note: We'll use the global Sonnet 4.5 profile instead
SANITIZED_PROFILE_NAME=$(echo "$USERNAME" | sed 's/@/-at-/g' | sed 's/\./-/g' | tr '[:upper:]' '[:lower:]')
SANITIZED_PROFILE_NAME="hackathon-${SANITIZED_PROFILE_NAME}"

echo ""
echo -e "${GREEN}You'll be using Claude Sonnet 4.5 (latest & greatest!)${NC}"
echo ""

# Check if Claude Code is installed
echo -e "${BLUE}Step 4: Checking Claude Code installation...${NC}"
echo ""

log_verbose "Checking for claude or claude-code commands..."
# Check if Claude Code is installed (can be 'claude' or 'claude-code' command)
if command -v claude &> /dev/null || command -v claude-code &> /dev/null; then
    CLAUDE_CMD="claude"
    command -v claude-code &> /dev/null && CLAUDE_CMD="claude-code"
    
    log_verbose "Found command: $CLAUDE_CMD"
    echo -e "${GREEN}? Claude Code already installed${NC}"
    
    # Verify it actually works
    log_verbose "Verifying command works..."
    if [ $VERBOSE -eq 1 ]; then
        CLAUDE_VERSION=$($CLAUDE_CMD --version 2>&1 | head -1)
        VERSION_EXIT_CODE=$?
        log_verbose "Version check exit code: $VERSION_EXIT_CODE"
    else
        CLAUDE_VERSION=$($CLAUDE_CMD --version 2>/dev/null | head -1)
        VERSION_EXIT_CODE=$?
    fi
    
    if [ $VERSION_EXIT_CODE -eq 0 ]; then
        echo -e "${GREEN}  Version: ${CLAUDE_VERSION}${NC}"
        echo -e "${GREEN}  Command: ${CLAUDE_CMD}${NC}"
    else
        echo -e "${YELLOW}  Command exists but not responding, may need manual cleanup${NC}"
        log_verbose "Version output: $CLAUDE_VERSION"
        echo -e "${YELLOW}  Run: npm uninstall -g @anthropic-ai/claude-code && npm install -g @anthropic-ai/claude-code${NC}"
    fi
else
    echo -e "${YELLOW}? Claude Code not found. Installing...${NC}"
    
    # Try regular install first
    log_verbose "Attempting npm install -g @anthropic-ai/claude-code..."
    if [ $VERBOSE -eq 1 ]; then
        NPM_OUTPUT=$(npm install -g @anthropic-ai/claude-code 2>&1)
        NPM_EXIT_CODE=$?
        echo "$NPM_OUTPUT"
    else
        NPM_OUTPUT=$(npm install -g @anthropic-ai/claude-code 2>&1)
        NPM_EXIT_CODE=$?
    fi
    
    if [ $NPM_EXIT_CODE -eq 0 ]; then
        echo -e "${GREEN}? Claude Code installed${NC}"
    else
        # If that fails, try to clean up first
        echo -e "${YELLOW}  Installation failed, attempting cleanup...${NC}"
        log_verbose "Install failed with exit code: $NPM_EXIT_CODE"
        [ $VERBOSE -eq 1 ] && echo "$NPM_OUTPUT"
        
        log_verbose "Running npm uninstall -g @anthropic-ai/claude-code..."
        if [ $VERBOSE -eq 1 ]; then
            npm uninstall -g @anthropic-ai/claude-code 2>&1
        else
            npm uninstall -g @anthropic-ai/claude-code 2>/dev/null
        fi
        
        # Now try install again
        log_verbose "Retrying npm install -g @anthropic-ai/claude-code..."
        if [ $VERBOSE -eq 1 ]; then
            RETRY_OUTPUT=$(npm install -g @anthropic-ai/claude-code 2>&1)
            RETRY_EXIT_CODE=$?
            echo "$RETRY_OUTPUT"
        else
            RETRY_OUTPUT=$(npm install -g @anthropic-ai/claude-code 2>&1)
            RETRY_EXIT_CODE=$?
        fi
        
        if [ $RETRY_EXIT_CODE -eq 0 ]; then
            echo -e "${GREEN}? Claude Code installed after cleanup${NC}"
        else
            echo -e "${RED}? Failed to install Claude Code${NC}"
            log_verbose "Retry failed with exit code: $RETRY_EXIT_CODE"
            [ $VERBOSE -eq 1 ] && echo "$RETRY_OUTPUT"
            echo ""
            echo -e "${YELLOW}Manual fix needed:${NC}"
            echo "  1. Clean up: npm uninstall -g @anthropic-ai/claude-code"
            echo "  2. Remove directory: rm -rf /opt/homebrew/lib/node_modules/@anthropic-ai/claude-code"
            echo "  3. Reinstall: npm install -g @anthropic-ai/claude-code"
            echo "  4. Run this script again with -v flag for more details"
            echo ""
            exit 1
        fi
    fi
fi

# Create AWS config if needed
echo ""
echo -e "${BLUE}Step 5: Configuring AWS region...${NC}"
echo ""

if [ ! -f ~/.aws/config ]; then
    cat > ~/.aws/config << EOF
[default]
region = ap-southeast-2
output = json
EOF
    echo -e "${GREEN}? Created ~/.aws/config${NC}"
else
    if ! grep -q "region = ap-southeast-2" ~/.aws/config; then
        sed -i '' 's/region = .*/region = ap-southeast-2/' ~/.aws/config 2>/dev/null || \
        echo -e "\n[default]\nregion = ap-southeast-2\noutput = json" >> ~/.aws/config
    fi
    echo -e "${GREEN}? Updated ~/.aws/config${NC}"
fi

# Use Claude Sonnet 4.5 via ap-southeast-2 inference profile
echo ""
echo -e "${BLUE}Step 6: Setting up Claude Sonnet 4.5...${NC}"
echo ""

# Use the pre-existing Claude Sonnet 4.5 inference profile in ap-southeast-2
PROFILE_NAME="Claude Sonnet 4.5"
INFERENCE_PROFILE_ARN="arn:aws:bedrock:ap-southeast-2:384553929753:application-inference-profile/9msh6tdlsg7d"

echo -e "${GREEN}? Using Claude Sonnet 4.5 in ap-southeast-2${NC}"
echo -e "${GREEN}  Inference Profile: $PROFILE_NAME${NC}"
echo -e "${GREEN}  ARN: $INFERENCE_PROFILE_ARN${NC}"
echo ""

# Skip the old inference profile creation code
SKIP_CREATION=1

# Old code below (kept for reference but skipped)
if false; then
PROFILE_EXISTS=$(check_inference_profile_exists "$PROFILE_NAME")

if [ "$PROFILE_EXISTS" = "yes" ]; then
    echo -e "${GREEN}? Inference profile '$PROFILE_NAME' already exists${NC}"
    INFERENCE_PROFILE_ARN=$(get_inference_profile_arn "$PROFILE_NAME")
    echo -e "${GREEN}  ARN: $INFERENCE_PROFILE_ARN${NC}"
else
    echo -e "${YELLOW}Creating new inference profile: $PROFILE_NAME${NC}"
    
    # Create inference profile JSON
    # Sanitize description (AWS only allows: [0-9a-zA-Z:.][ _-]?)
    SANITIZED_USERNAME=$(echo "$USERNAME" | sed 's/@/ at /g' | sed 's/\./ dot /g')
    
    cat > /tmp/hackathon-inference-profile.json << EOF
{
    "inferenceProfileName": "$PROFILE_NAME",
    "description": "Hackathon profile for $SANITIZED_USERNAME",
    "modelSource": {
        "copyFrom": "arn:aws:bedrock:us-west-2:$AWS_ACCOUNT:inference-profile/us.anthropic.claude-3-5-sonnet-20241022-v2:0"
    },
    "tags": [
        {
            "key": "Application",
            "value": "hackathon-claude-code"
        },
        {
            "key": "ITBU",
            "value": "dai"
        },
        {
            "key": "Username",
            "value": "$USERNAME"
        },
        {
            "key": "CreatedBy",
            "value": "hackathon-setup-script"
        }
    ]
}
EOF

    # Create the inference profile
    CREATE_RESULT=$(aws bedrock create-inference-profile --region us-west-2 --cli-input-json file:///tmp/hackathon-inference-profile.json 2>&1)
    CREATE_EXIT_CODE=$?
    
    # Clean up temp file
    rm /tmp/hackathon-inference-profile.json 2>/dev/null
    
    if [ $CREATE_EXIT_CODE -eq 0 ]; then
        # Try to parse the ARN
        INFERENCE_PROFILE_ARN=$(echo "$CREATE_RESULT" | python3 -c "import sys, json; data = json.load(sys.stdin); print(data.get('inferenceProfileArn', ''))" 2>/dev/null)
        
        if [ -n "$INFERENCE_PROFILE_ARN" ]; then
            echo -e "${GREEN}? Created inference profile${NC}"
            echo -e "${GREEN}  ARN: $INFERENCE_PROFILE_ARN${NC}"
        else
            echo -e "${YELLOW}? Profile created but couldn't parse ARN${NC}"
            echo -e "${YELLOW}  Falling back to direct model ID${NC}"
            INFERENCE_PROFILE_ARN="anthropic.claude-3-5-sonnet-20241022-v2:0"
        fi
    else
        echo -e "${YELLOW}? Could not create inference profile${NC}"
        
        # Check for common errors
        if echo "$CREATE_RESULT" | grep -qi "AlreadyExists\|ConflictException"; then
            echo -e "${YELLOW}  Reason: Profile may already exist${NC}"
            echo -e "${YELLOW}  Attempting to find existing profile...${NC}"
            
            # Try to get existing profile
            EXISTING_ARN=$(aws bedrock list-inference-profiles --region us-west-2 --output json 2>/dev/null | \
                python3 -c "import sys, json; data = json.load(sys.stdin); profiles = [p for p in data.get('inferenceProfileSummaries', []) if p.get('inferenceProfileName') == '$PROFILE_NAME']; print(profiles[0]['inferenceProfileArn'] if profiles else '')" 2>/dev/null)
            
            if [ -n "$EXISTING_ARN" ]; then
                INFERENCE_PROFILE_ARN="$EXISTING_ARN"
                echo -e "${GREEN}? Found existing profile${NC}"
                echo -e "${GREEN}  ARN: $INFERENCE_PROFILE_ARN${NC}"
            else
                echo -e "${YELLOW}  Could not find existing profile, using direct model ID${NC}"
                INFERENCE_PROFILE_ARN="anthropic.claude-3-5-sonnet-20241022-v2:0"
            fi
        elif echo "$CREATE_RESULT" | grep -qi "AccessDenied\|Unauthorized"; then
            echo -e "${YELLOW}  Reason: Insufficient permissions${NC}"
            echo -e "${YELLOW}  Using direct model ID instead${NC}"
            INFERENCE_PROFILE_ARN="anthropic.claude-3-5-sonnet-20241022-v2:0"
        else
            echo -e "${YELLOW}  Reason: Unknown error${NC}"
            echo -e "${YELLOW}  Error details: $(echo "$CREATE_RESULT" | head -3)${NC}"
            echo -e "${YELLOW}  Using direct model ID instead${NC}"
            INFERENCE_PROFILE_ARN="anthropic.claude-3-5-sonnet-20241022-v2:0"
        fi
    fi
fi
fi  # Close the `if false; then` block
# End of old inference profile creation code

# Setup MCPs (Model Context Protocol tools)
echo ""
echo -e "${BLUE}Step 7: Setting up MCP tools...${NC}"
echo ""
echo -e "${YELLOW}Ok. Now we are going to set you up with a couple of tools via MCP.${NC}"
echo -e "${YELLOW}This won't take a hot minute!${NC}"
echo ""

# Playwright MCP
echo -e "${GREEN}1. Playwright${NC} - Browser automation for testing web apps"
read -p "   Install Playwright MCP? (Y/n): " INSTALL_PLAYWRIGHT
if [[ ! "$INSTALL_PLAYWRIGHT" =~ ^[Nn] ]]; then
    echo "   Installing Playwright MCP..."
    log_verbose "Running: claude mcp add playwright npx @playwright/mcp@latest"
    
    if [ $VERBOSE -eq 1 ]; then
        MCP_OUTPUT=$(claude mcp add playwright npx @playwright/mcp@latest 2>&1)
        MCP_EXIT_CODE=$?
        echo "$MCP_OUTPUT"
    else
        MCP_OUTPUT=$(claude mcp add playwright npx @playwright/mcp@latest 2>&1)
        MCP_EXIT_CODE=$?
    fi
    
    if echo "$MCP_OUTPUT" | grep -q "Added\|Successfully" || [ $MCP_EXIT_CODE -eq 0 ]; then
        echo -e "   ${GREEN}? Playwright MCP installed${NC}"
    else
        echo -e "   ${YELLOW}? Playwright MCP installation may have issues (non-fatal)${NC}"
        log_verbose "Exit code: $MCP_EXIT_CODE"
        [ $VERBOSE -eq 1 ] || echo "$MCP_OUTPUT"
    fi
else
    echo "   Skipped"
fi
echo ""

# Chrome DevTools MCP
echo -e "${GREEN}2. Chrome DevTools${NC} - Debug and inspect web applications"
read -p "   Install Chrome DevTools MCP? (Y/n): " INSTALL_CHROME
if [[ ! "$INSTALL_CHROME" =~ ^[Nn] ]]; then
    echo "   Installing Chrome DevTools MCP..."
    log_verbose "Running: claude mcp add chrome-devtools npx chrome-devtools-mcp@latest"
    
    if [ $VERBOSE -eq 1 ]; then
        MCP_OUTPUT=$(claude mcp add chrome-devtools npx chrome-devtools-mcp@latest 2>&1)
        MCP_EXIT_CODE=$?
        echo "$MCP_OUTPUT"
    else
        MCP_OUTPUT=$(claude mcp add chrome-devtools npx chrome-devtools-mcp@latest 2>&1)
        MCP_EXIT_CODE=$?
    fi
    
    if echo "$MCP_OUTPUT" | grep -q "Added\|Successfully" || [ $MCP_EXIT_CODE -eq 0 ]; then
        echo -e "   ${GREEN}? Chrome DevTools MCP installed${NC}"
    else
        echo -e "   ${YELLOW}? Chrome DevTools MCP installation may have issues (non-fatal)${NC}"
        log_verbose "Exit code: $MCP_EXIT_CODE"
        [ $VERBOSE -eq 1 ] || echo "$MCP_OUTPUT"
    fi
else
    echo "   Skipped"
fi
echo ""

# GitHub MCP
echo -e "${GREEN}3. GitHub${NC} - Access repositories, issues, and PRs"
echo -e "   ${YELLOW}Note: Requires a Personal Access Token (PAT)${NC}"
echo -e "   ${YELLOW}Create at: https://github.com/settings/tokens${NC}"
echo -e "   ${YELLOW}Recommended scopes: repo (least privilege)${NC}"
echo ""
read -p "   Install GitHub MCP? (y/N): " INSTALL_GITHUB
if [[ "$INSTALL_GITHUB" =~ ^[Yy] ]]; then
    echo ""
    echo -e "   ${BLUE}GitHub PAT Setup:${NC}"
    echo "   1. Go to: https://github.com/settings/tokens"
    echo "   2. Click 'Generate new token' ? 'Generate new token (classic)'"
    echo "   3. Add note: 'Claude Code MCP'"
    echo "   4. Select scopes: 'repo' (for full repository access)"
    echo "   5. Or use fine-grained for least privilege:"
    echo "      - Repository access: Only select repositories"
    echo "      - Permissions: Contents (Read), Issues (Read/Write), Pull requests (Read/Write)"
    echo ""
    read -p "   Enter your GitHub PAT (or press Enter to skip): " GITHUB_PAT
    
    if [ -n "$GITHUB_PAT" ]; then
        echo "   Installing GitHub MCP..."
        log_verbose "Running: claude mcp add --transport http github https://api.githubcopilot.com/mcp -H \"Authorization: Bearer ***\""
        
        if [ $VERBOSE -eq 1 ]; then
            MCP_OUTPUT=$(claude mcp add --transport http github https://api.githubcopilot.com/mcp -H "Authorization: Bearer $GITHUB_PAT" 2>&1)
            MCP_EXIT_CODE=$?
            # Redact token in output
            echo "$MCP_OUTPUT" | sed "s/$GITHUB_PAT/***/g"
        else
            MCP_OUTPUT=$(claude mcp add --transport http github https://api.githubcopilot.com/mcp -H "Authorization: Bearer $GITHUB_PAT" 2>&1)
            MCP_EXIT_CODE=$?
        fi
        
        if echo "$MCP_OUTPUT" | grep -q "Added\|Successfully" || [ $MCP_EXIT_CODE -eq 0 ]; then
            echo -e "   ${GREEN}? GitHub MCP installed${NC}"
        else
            echo -e "   ${YELLOW}? GitHub MCP installation may have issues (non-fatal)${NC}"
            log_verbose "Exit code: $MCP_EXIT_CODE"
            [ $VERBOSE -eq 1 ] || echo "$MCP_OUTPUT" | sed "s/$GITHUB_PAT/***/g"
        fi
    else
        echo "   Skipped (no PAT provided)"
    fi
else
    echo "   Skipped"
fi
echo ""

# Detect shell config files - write to BOTH zsh and bash configs if they exist
echo ""
echo -e "${BLUE}Step 8: Configuring shell environment...${NC}"
echo ""

# Collect all shell config files that exist
SHELL_CONFIGS=()
[ -f "$HOME/.zshrc" ] && SHELL_CONFIGS+=("$HOME/.zshrc")
[ -f "$HOME/.bashrc" ] && SHELL_CONFIGS+=("$HOME/.bashrc")
[ -f "$HOME/.bash_profile" ] && SHELL_CONFIGS+=("$HOME/.bash_profile")

if [ ${#SHELL_CONFIGS[@]} -eq 0 ]; then
    echo -e "${RED}? Could not find any shell config files${NC}"
    echo "Checked: ~/.zshrc, ~/.bashrc, ~/.bash_profile"
    exit 1
fi

echo -e "${GREEN}? Found ${#SHELL_CONFIGS[@]} shell config file(s):${NC}"
for config in "${SHELL_CONFIGS[@]}"; do
    echo "   $config"
done
echo ""

# Add/update configuration in each found config file
for SHELL_CONFIG in "${SHELL_CONFIGS[@]}"; do
    if ! grep -q "CLAUDE_CODE_USE_BEDROCK" "$SHELL_CONFIG" 2>/dev/null; then
        # Add new configuration
        cat >> "$SHELL_CONFIG" << EOF

# ============================================================
# HACKATHON: Claude Code + AWS Bedrock Configuration
# Created: $(date)
# To disable: Run hackathon-undo.sh
# ============================================================
export CLAUDE_CODE_USE_BEDROCK=1
export AWS_REGION=ap-southeast-2
export ANTHROPIC_MODEL='$INFERENCE_PROFILE_ARN'
export ANTHROPIC_SMALL_FAST_MODEL='au.anthropic.claude-haiku-4-5-20251001-v1:0'
# ============================================================
EOF
        echo -e "${GREEN}? Added Bedrock configuration to $(basename $SHELL_CONFIG)${NC}"
    else
        echo -e "${YELLOW}? Bedrock configuration already exists in $(basename $SHELL_CONFIG)${NC}"
        echo "  Updating to use ap-southeast-2 and Claude Sonnet 4.5..."
        
        # Update the AWS_REGION line
        sed -i '' "s|export AWS_REGION=.*|export AWS_REGION=ap-southeast-2|g" "$SHELL_CONFIG"
        
        # Update the ANTHROPIC_MODEL line
        sed -i '' "s|export ANTHROPIC_MODEL=.*|export ANTHROPIC_MODEL='$INFERENCE_PROFILE_ARN'|g" "$SHELL_CONFIG"
        
        echo -e "${GREEN}? Updated $(basename $SHELL_CONFIG)${NC}"
    fi
done

# Use the primary shell config for sourcing (based on user's default shell)
PRIMARY_SHELL_CONFIG=""
case "$SHELL" in
    */zsh) PRIMARY_SHELL_CONFIG="$HOME/.zshrc" ;;
    */bash) 
        PRIMARY_SHELL_CONFIG="$HOME/.bash_profile"
        [ ! -f "$PRIMARY_SHELL_CONFIG" ] && PRIMARY_SHELL_CONFIG="$HOME/.bashrc"
        ;;
esac
[ -z "$PRIMARY_SHELL_CONFIG" ] && PRIMARY_SHELL_CONFIG="${SHELL_CONFIGS[0]}"

# Create undo script
echo ""
echo -e "${BLUE}Step 8: Creating undo script...${NC}"
echo ""

cat > "$(dirname "$0")/hackathon-undo.sh" << 'UNDO_SCRIPT'
#!/bin/bash

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

echo -e "${BLUE}+------------------------------------------------------------+${NC}"
echo -e "${BLUE}¦          Hackathon Setup Undo Script                      ¦${NC}"
echo -e "${BLUE}+------------------------------------------------------------+${NC}"
echo ""

# Find all shell config files that might have Bedrock config
SHELL_CONFIGS=()
[ -f "$HOME/.zshrc" ] && grep -q "CLAUDE_CODE_USE_BEDROCK" "$HOME/.zshrc" 2>/dev/null && SHELL_CONFIGS+=("$HOME/.zshrc")
[ -f "$HOME/.bashrc" ] && grep -q "CLAUDE_CODE_USE_BEDROCK" "$HOME/.bashrc" 2>/dev/null && SHELL_CONFIGS+=("$HOME/.bashrc")
[ -f "$HOME/.bash_profile" ] && grep -q "CLAUDE_CODE_USE_BEDROCK" "$HOME/.bash_profile" 2>/dev/null && SHELL_CONFIGS+=("$HOME/.bash_profile")

if [ ${#SHELL_CONFIGS[@]} -eq 0 ]; then
    echo -e "${YELLOW}? No Bedrock configuration found in any shell config files${NC}"
    exit 0
fi

echo -e "${YELLOW}This will remove Bedrock configuration from:${NC}"
for config in "${SHELL_CONFIGS[@]}"; do
    echo "   $config"
done
echo ""
read -p "Continue? (y/N): " CONFIRM

if [[ ! "$CONFIRM" =~ ^[Yy] ]]; then
    echo "Cancelled."
    exit 0
fi

# Process each config file
for SHELL_CONFIG in "${SHELL_CONFIGS[@]}"; do
    echo ""
    echo -e "${BLUE}Processing $(basename $SHELL_CONFIG)...${NC}"
    
    # Backup the config file
    cp "$SHELL_CONFIG" "${SHELL_CONFIG}.backup-$(date +%Y%m%d-%H%M%S)"
    echo -e "${GREEN}? Created backup${NC}"
    
    # Remove the Bedrock configuration - use multiple strategies
    # Strategy 1: Remove blocks between markers (any length of ===)
    sed -i '' '/# =\{40,\}$/,/# =\{40,\}$/{
        /^# HACKATHON:/,/# =\{40,\}$/d
    }' "$SHELL_CONFIG" 2>/dev/null || true
    
    # Strategy 2: Explicitly remove any remaining Bedrock variables
    sed -i '' '/^export CLAUDE_CODE_USE_BEDROCK/d' "$SHELL_CONFIG"
    sed -i '' '/^export AWS_REGION=ap-southeast/d' "$SHELL_CONFIG"
    sed -i '' '/^export ANTHROPIC_MODEL=.*bedrock/d' "$SHELL_CONFIG"
    sed -i '' '/^export ANTHROPIC_SMALL_FAST_MODEL=.*anthropic\.claude/d' "$SHELL_CONFIG"
    sed -i '' '/^# export CLAUDE_CODE_USE_BEDROCK/d' "$SHELL_CONFIG"
    sed -i '' '/^# export AWS_REGION=ap-southeast/d' "$SHELL_CONFIG"
    sed -i '' '/^# export ANTHROPIC_MODEL=.*bedrock/d' "$SHELL_CONFIG"
    sed -i '' '/^# export ANTHROPIC_SMALL_FAST_MODEL=.*anthropic\.claude/d' "$SHELL_CONFIG"
    
    # Strategy 3: Remove comment lines
    sed -i '' '/^# HACKATHON: Claude Code/d' "$SHELL_CONFIG"
    sed -i '' '/^# Created: .* AEDT/d' "$SHELL_CONFIG"
    sed -i '' '/^# To disable: Run hackathon-undo/d' "$SHELL_CONFIG"
    sed -i '' '/^# =\{40,\}$/d' "$SHELL_CONFIG"
    
    # Remove any blank lines at the end
    sed -i '' -e :a -e '/^\n*$/{$d;N;ba' -e '}' "$SHELL_CONFIG" 2>/dev/null || true
    
    echo -e "${GREEN}? Removed Bedrock configuration from $(basename $SHELL_CONFIG)${NC}"
done

echo ""
echo -e "${GREEN}? Undo complete!${NC}"
echo ""
echo "To activate changes:"
echo "  1. Close this terminal and open a new one"
echo "  2. Or run: unset CLAUDE_CODE_USE_BEDROCK AWS_REGION ANTHROPIC_MODEL ANTHROPIC_SMALL_FAST_MODEL"
echo ""
echo "Claude Code will now use its default configuration (Claude.ai account)"
echo ""
UNDO_SCRIPT

chmod +x "$(dirname "$0")/hackathon-undo.sh"
echo -e "${GREEN}? Created hackathon-undo.sh${NC}"

# Export for current shell
export CLAUDE_CODE_USE_BEDROCK=1
export AWS_REGION=us-west-2
export ANTHROPIC_MODEL="$INFERENCE_PROFILE_ARN"
export ANTHROPIC_SMALL_FAST_MODEL='anthropic.claude-3-5-haiku-20241022-v1:0'

# Check if we're using the fallback model (3.5)
if [[ "$INFERENCE_PROFILE_ARN" == "anthropic.claude-3-5-sonnet-20241022-v2:0" ]]; then
    echo ""
    echo -e "${RED}+---------------------------------------------------------------+${NC}"
    echo -e "${RED}¦                    ??  CRITICAL WARNING ??                     ¦${NC}"
    echo -e "${RED}¦---------------------------------------------------------------¦${NC}"
    echo -e "${RED}¦  Claude Code requires Claude 4.5 (Sonnet)                    ¦${NC}"
    echo -e "${RED}¦  Currently falling back to Claude 3.5 which is NOT supported ¦${NC}"
    echo -e "${RED}¦                                                               ¦${NC}"
    echo -e "${RED}¦  ${YELLOW}ACTION REQUIRED:${RED} Contact your admin to fix SCP          ¦${NC}"
    echo -e "${RED}¦  See: talktoadmin.md                                         ¦${NC}"
    echo -e "${RED}¦                                                               ¦${NC}"
    echo -e "${RED}¦  Setup will continue but Claude Code may not work properly   ¦${NC}"
    echo -e "${RED}+---------------------------------------------------------------+${NC}"
    echo ""
    read -p "Press Enter to continue anyway..."
    echo ""
fi

# Test AWS Bedrock access
echo ""
echo -e "${BLUE}Step 9: Testing Bedrock access...${NC}"
echo ""

log_verbose "Testing AWS Bedrock access with model: $INFERENCE_PROFILE_ARN"
log_verbose "Running: aws bedrock-runtime invoke-model --region us-west-2 --model-id $INFERENCE_PROFILE_ARN ..."

if [ $VERBOSE -eq 1 ]; then
    TEST_RESULT=$(aws bedrock-runtime invoke-model \
      --region us-west-2 \
      --model-id "$INFERENCE_PROFILE_ARN" \
      --body '{"anthropic_version":"bedrock-2023-05-31","messages":[{"role":"user","content":"Hi"}],"max_tokens":5}' \
      --cli-binary-format raw-in-base64-out \
      /tmp/bedrock-test.json 2>&1)
    TEST_EXIT_CODE=$?
    log_verbose "Bedrock test exit code: $TEST_EXIT_CODE"
    log_verbose "Bedrock test output:"
    echo "$TEST_RESULT"
else
    TEST_RESULT=$(aws bedrock-runtime invoke-model \
      --region us-west-2 \
      --model-id "$INFERENCE_PROFILE_ARN" \
      --body '{"anthropic_version":"bedrock-2023-05-31","messages":[{"role":"user","content":"Hi"}],"max_tokens":5}' \
      --cli-binary-format raw-in-base64-out \
      /tmp/bedrock-test.json 2>&1)
    TEST_EXIT_CODE=$?
fi

if [ $TEST_EXIT_CODE -eq 0 ]; then
    echo -e "${GREEN}? Bedrock access working!${NC}"
    log_verbose "Response file contents:"
    [ $VERBOSE -eq 1 ] && cat /tmp/bedrock-test.json 2>&1
    rm /tmp/bedrock-test.json 2>/dev/null
else
    echo -e "${YELLOW}? Bedrock test failed (non-fatal)${NC}"
    log_verbose "Full error output:"
    [ $VERBOSE -eq 1 ] && echo "$TEST_RESULT"
    
    # Show abbreviated error
    ERROR_MSG=$(echo "$TEST_RESULT" | grep -oE "(AccessDeniedException|ValidationException|UnauthorizedException)" | head -1)
    log_verbose "Extracted error type: ${ERROR_MSG:-none}"
    
    if echo "$TEST_RESULT" | grep -qi "AccessDeniedException.*service control policy"; then
        echo -e "${YELLOW}  Reason: SCP restrictions block inference profile${NC}"
        echo -e "${YELLOW}  This is EXPECTED - see talktoadmin.md${NC}"
        echo ""
        echo -e "${GREEN}  Setup will continue - you can use direct model access${NC}"
    elif echo "$TEST_RESULT" | grep -qi "ValidationException"; then
        echo -e "${YELLOW}  Reason: Model validation error${NC}"
        echo -e "${YELLOW}  Your inference profile may work once SCP is updated${NC}"
    elif [ -n "$ERROR_MSG" ]; then
        echo -e "${YELLOW}  Reason: $ERROR_MSG${NC}"
    else
        echo -e "${YELLOW}  Error: $(echo "$TEST_RESULT" | head -1)${NC}"
    fi
    
    rm /tmp/bedrock-test.json 2>/dev/null
    echo ""
    echo -e "${GREEN}Continuing with setup...${NC}"
fi

# Final summary with animated logo
echo ""
for color in "${GREEN}" "${BLUE}" "${PURPLE}"; do
    clear 2>/dev/null || true
    echo -e "${color}"
    cat << 'EOF'
                ######                                      ####### #######
          ##    ######    ##                                ####### #######
        #####   ######  ######                              ####### #######
       ######## ###### ########                                     #######
        ######## ####  #######        ###### ##########     ####### ##################
          ######       #####          ###################   ####### ####################
     #################     #######    ####################  ####### ######################
     #################   #########    #######      #######  ####### #########      ########
EOF
    echo -e "${NC}"
    sleep 0.15
done
echo -e "${GREEN}"
cat << 'EOF'
                ######                                      ####### #######
          ##    ######    ##                                ####### #######
        #####   ######  ######                              ####### #######
       ######## ###### ########                                     #######
        ######## ####  #######        ###### ##########     ####### ##################
          ######       #####          ###################   ####### ####################
     #################     #######    ####################  ####### ######################
     #################   #########    #######      #######  ####### #########      ########
EOF
echo -e "${NC}"
echo ""
echo -e "${PURPLE}+------------------------------------------------------------+${NC}"
echo -e "${PURPLE}¦                    Setup Complete!                         ¦${NC}"
echo -e "${PURPLE}+------------------------------------------------------------+${NC}"
echo ""
echo -e "${GREEN}? Claude Code installed and configured${NC}"
echo -e "${GREEN}? AWS Bedrock integration ready${NC}"
echo -e "${GREEN}? Inference profile: $PROFILE_NAME${NC}"
echo ""

# Load the environment variables in the current shell
echo -e "${YELLOW}??  Loading environment variables...${NC}"
source "$PRIMARY_SHELL_CONFIG"
echo -e "${GREEN}? Environment variables loaded in current terminal${NC}"
echo ""

# Verify the variables are set
if [ -n "$CLAUDE_CODE_USE_BEDROCK" ]; then
    echo -e "${GREEN}? CLAUDE_CODE_USE_BEDROCK is set${NC}"
else
    echo -e "${RED}??  Warning: CLAUDE_CODE_USE_BEDROCK not set. Try: source $PRIMARY_SHELL_CONFIG${NC}"
fi
echo ""

echo -e "${BLUE}??????????????????????????????????????????????????????????${NC}"
echo ""
echo -e "${YELLOW}?? Next Steps:${NC}"
echo ""
echo -e "1. ${BLUE}Start Claude Code in THIS terminal:${NC}"
echo "   \$ claude"
echo "   (or: claude-code depending on your version)"
echo ""
echo -e "   ${GREEN}?${NC} Variables are already loaded in this terminal!"
echo ""
echo -e "2. ${BLUE}For OTHER/NEW terminals:${NC}"
echo -e "   Close Terminal app completely and reopen it"
echo -e "   ${YELLOW}(New tabs/windows may not work - close the app!)${NC}"
echo ""
echo -e "   Or manually load variables:"
echo "   \$ source $PRIMARY_SHELL_CONFIG"
echo ""
echo -e "3. ${BLUE}When RQP credentials expire (in $REMAINING min):${NC}"
echo "   Your AWS session will timeout after ~1 hour"
echo ""
   echo -e "   ${YELLOW}Symptoms:${NC} Claude shows authentication/permission errors"
   echo -e "   ${GREEN}Fix:${NC} Re-authenticate and restart:"
   echo ""
   echo -e "   \$ rqp auth -r poweruser -s kaos -z secure    ${BLUE}# Get fresh credentials${NC}"
   echo -e "   \$ claude                                      ${BLUE}# Start coding again${NC}"
echo ""
echo -e "   ${YELLOW}Note:${NC} AWS credentials update automatically from ~/.aws/credentials"
echo ""
echo -e "${BLUE}??????????????????????????????????????????????????????????${NC}"
echo ""
echo -e "${YELLOW}??  Important: Credential Expiry${NC}"
echo "   RQP credentials expire after ~1 hour (current: $REMAINING min left)"
echo -e "   Remember to run ${GREEN}rqp auth -r poweruser -s kaos -z secure${NC} when you see authentication errors"
echo ""
echo -e "${BLUE}??????????????????????????????????????????????????????????${NC}"
echo ""
echo -e "${YELLOW}?? To Undo This Setup (Revert to Claude Max):${NC}"
echo ""
echo "   \$ ./hackathon-undo.sh"
echo ""
echo "   This will:"
echo -e "   ${GREEN}${NC} Backup your shell config"
echo -e "   ${GREEN}${NC} Remove all Bedrock environment variables"
echo -e "   ${GREEN}${NC} Restore Claude Code to use your Max subscription"
echo -e "   ${GREEN}${NC} Preserve all your other settings"
echo ""
echo "   After running undo, restart Claude Code and it will use your Max subscription."
echo ""
echo -e "${BLUE}??????????????????????????????????????????????????????????${NC}"
echo ""
echo -e "${YELLOW}?? Configuration Details:${NC}"
echo "   Primary Model: Claude Sonnet 4.5"
echo "   Fast Model: Claude Haiku 4.5"
echo "   Region: ap-southeast-2"
echo "   Profile: $PROFILE_NAME"
echo "   User: $USERNAME"
echo "   Command: claude (or claude-code)"
echo ""
echo -e "${YELLOW}?? Quick Reference:${NC}"
echo -e "   Check credentials: ${GREEN}aws sts get-caller-identity${NC}"
echo -e "   Refresh credentials: ${GREEN}rqp auth -r poweruser -s kaos -z secure${NC}"
echo -e "   Check setup: ${GREEN}./check-claude-mode.sh${NC}"
echo -e "   Run setup with verbose errors: ${GREEN}./hackathon-setup.sh -v${NC}"
echo ""
echo -e "${GREEN}Happy Hacking! ??${NC}"
echo ""

