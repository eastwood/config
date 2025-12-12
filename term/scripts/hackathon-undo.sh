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
