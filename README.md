# Decentralized Credit Scoring System

A Clarity smart contract that enables on-chain credit scoring based on financial history and behavior.

## Overview

This contract implements a decentralized credit scoring system that:

1. Tracks users' financial history on the blockchain
2. Calculates credit scores based on configurable factors
3. Allows authorized reporters to submit financial data
4. Provides transparent access to credit scores

## Contract Details

The credit scoring system uses a multi-factor approach where different aspects of a user's financial history contribute to their overall score. The contract owner can define and adjust these factors and their weights.

## Key Features

- User registration
- Financial history recording
- Configurable scoring factors with weights
- Authorized reporter system
- Credit score calculation and retrieval
- Administrative controls

## Usage

### For Users

Register yourself in the system:
```clarity
(contract-call? .credit-scoring register-user)
```

Check your credit score:
```clarity
(contract-call? .credit-scoring get-credit-score tx-sender)
```

### For Authorized Reporters

Add a financial record for a user:
```clarity
(contract-call? .credit-scoring add-financial-record 
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM 
  u1000 
  "loan-repayment" 
  (some 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG))
```

Update a user's factor score:
```clarity
(contract-call? .credit-scoring update-factor-score
  'ST1PQHQKV0RJXZFY1DGX8MNSNYVE3VGZJSRTPGZGM
  u1
  u750)
```

### For Contract Owner

Add a scoring factor:
```clarity
(contract-call? .credit-scoring add-score-factor u1 "payment-history" u8)
```

Authorize a reporter:
```clarity
(contract-call? .credit-scoring authorize-reporter 'ST2CY5V39NHDPWSXMW9QDT3HC3GD6Q6XX4CFRK9AG)
```

Update algorithm version:
```clarity
(contract-call? .credit-scoring update-algorithm-version u2)
```

## Error Codes

- `u100`: Owner only function
- `u101`: Entity not found
- `u102`: Entity already exists
- `u103`: Invalid score value
- `u104`: Invalid weight value
- `u105`: Invalid address
- `u106`: Unauthorized access

## Score Range

Credit scores range from 0 to 850, with a default starting score of 500.

