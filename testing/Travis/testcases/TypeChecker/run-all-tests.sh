#!/bin/bash

# Type Checker Test Runner
# Automatically runs all type checker tests in shouldPass and shouldFail directories
# Usage: ./run-all-tests.sh

set -e

# Color codes for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Test counters
PASS_COUNT=0
FAIL_COUNT=0
TOTAL_COUNT=0

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

echo "========================================"
echo "Type Checker Test Suite"
echo "========================================"
echo ""

# Test shouldPass files (expect exit code 0)
echo "Testing shouldPass files..."
echo "----------------------------------------"
if [ -d "$SCRIPT_DIR/shouldPass" ]; then
    for test_file in "$SCRIPT_DIR/shouldPass"/*.adl; do
        if [ -f "$test_file" ]; then
            TOTAL_COUNT=$((TOTAL_COUNT + 1))
            test_name=$(basename "$test_file")
            
            if ampersand check "$test_file" > /dev/null 2>&1; then
                echo -e "${GREEN}✓${NC} PASS: $test_name"
                PASS_COUNT=$((PASS_COUNT + 1))
            else
                echo -e "${RED}✗${NC} FAIL: $test_name (expected to pass but failed)"
                FAIL_COUNT=$((FAIL_COUNT + 1))
            fi
        fi
    done
else
    echo "Warning: shouldPass directory not found"
fi

echo ""

# Test shouldFail files (expect exit code 10)
echo "Testing shouldFail files..."
echo "----------------------------------------"
if [ -d "$SCRIPT_DIR/shouldFail" ]; then
    for test_file in "$SCRIPT_DIR/shouldFail"/*.adl; do
        if [ -f "$test_file" ]; then
            TOTAL_COUNT=$((TOTAL_COUNT + 1))
            test_name=$(basename "$test_file")
            
            # Run ampersand export and capture exit code
            if ampersand export "$test_file" > /dev/null 2>&1; then
                exit_code=$?
            else
                exit_code=$?
            fi
            
            if [ $exit_code -eq 10 ]; then
                echo -e "${GREEN}✓${NC} PASS: $test_name (correctly rejected)"
                PASS_COUNT=$((PASS_COUNT + 1))
            else
                echo -e "${RED}✗${NC} FAIL: $test_name (expected exit code 10, got $exit_code)"
                FAIL_COUNT=$((FAIL_COUNT + 1))
            fi
        fi
    done
else
    echo "Warning: shouldFail directory not found"
fi

echo ""
echo "========================================"
echo "Test Summary"
echo "========================================"
echo "Total tests: $TOTAL_COUNT"
echo -e "Passed: ${GREEN}$PASS_COUNT${NC}"
echo -e "Failed: ${RED}$FAIL_COUNT${NC}"
echo ""

if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed.${NC}"
    exit 1
fi
