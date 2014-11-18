### Stellar api bindings for Haskell

#### Notes
These bindings provide access to the [Stellar](https://www.stellar.org/) API. The following examples
assume a working GHCi session. Use of unsafe functions like `fromJust` and `head` are
used for brevity.

You'll need to set OverloadedStrings in GHCi:

```haskell
> :set -XOverloadedStrings
```

### Current Functionality

* [Testing connectivity to stellard](#ping)
* [Viewing account information](#account_info)
* [Viewing trust lines for an account](#account_trust_lines)
* [Viewing account transations](#account_transactions)
* [Submitting payments](#account_make_payment)

<a name="ping"></a>

### Pinging stellard

```haskell
> import Web.Stellar.Request

> pingStellar "https://test.stellar.org:9002"
Just PingSuccess

> pingStellar "https://google.com"
Just PingFailure
```

<a name="account_info"></a>

### Viewing Account Information

For further details on what these fields mean, consult the [Stellar
documentation](https://www.stellar.org/api/#api-account_info).

```haskell
> import Web.Stellar.Account
> import Data.Maybe
> import Control.Lens

> r <- fetchAccount "https://test.stellar.org:9002" "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb"

> let account = fromJust r

> account ^. balance
Just 135013248.000000000000

> account ^. flags
131072

> account ^. ownerCount
23

> account ^. previousTxnID 
"B5B8DD8668321C987C18F6131269455AA4CD6FD9E8AE6E5C4EA26BA9EDF7F487"

> account ^. previousTxnLgrSeq 
508838

> account ^. stellarSequence 
4265

> account ^. stellarIndex 
"6047FB9C7976F2D0554618F5ABFF423E7136205BAF19E92BE9D295E549442C45"
```

<a name="account_trust_lines"></a>

### Viewing Account Lines

[API documentation for account lines](https://www.stellar.org/api/#api-account_lines)

```haskell
> import Web.Stellar.AccountLine

> r <- fetchAccountLines "https://test.stellar.org:9002" "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb"

> let line = (Prelude.head.fromJust) r

> line ^. otherAccount
"ghj4kXtHfQcCaLQwpLJ11q2hq6248R7k9C"

> line ^. balance
Just 360.000000000000

> line ^. currency
"BTC"

> line ^. limit
Just 0.000000000000

> line ^. limitPeer
Just 0.000000000000
```

<a name="account_transactions"></a>

### Viewing Transactions for an Account

Here, 0 and -1 are used to set the boundaries of transaction history. 0 is the earliest ledger to fetch
from, -1 is the latest to fetch from (-1 signifying the current ledger). See the
[docs](https://www.stellar.org/api/#api-account_tx) for more.


```haskell
> import Web.Stellar.Transaction

> r <- fetchTransactions "https://test.stellar.org:9002" "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb" 0 (-1)

> let transaction = (Prelude.head.fromJust) r

> transaction ^. transactionAccount
"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb"

> transaction ^. destination
Just "gHJPw9kW8v4BsUyDnBR8ZHWo8aEkhUMeAq"

> transaction ^. signingPubKey
"BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D4"

> transaction ^. transactionType
"Payment"

> transaction ^. transactionSignature 
"82F10F2144AA2D10B888F5E7C69D9669CD9E126EFDE3ED8704EF5FB2C70ABE79FCE787B62D8C49001527B08E0655F9A624D5D33A3CA96A8ABAE86353811C5607"

> transaction ^. date
469441410

> transaction ^. hash
"B5B8DD8668321C987C18F6131269455AA4CD6FD9E8AE6E5C4EA26BA9EDF7F487"

> transaction ^. amount
Just 1000000000.000000000000

-- Empty currency from the API implies microstellars
> transaction ^. currency
""

> transaction ^. rawTransaction
"{\"TransactionType\":\"Payment\",\"Amount\":\"1000000000\",\"Destination\":\"gHJPw9kW8v4BsUyDnBR8ZHWo8aEkhUMeAq\",\"Flags\":2147483648,\"hash\":\"B5B8DD8668321C987C18F6131269455AA4CD6FD9E8AE6E5C4EA26BA9EDF7F487\",\"inLedger\":508838,\"TxnSignature\":\"82F10F2144AA2D10B888F5E7C69D9669CD9E126EFDE3ED8704EF5FB2C70ABE79FCE787B62D8C49001527B08E0655F9A624D5D33A3CA96A8ABAE86353811C5607\",\"Fee\":\"10\",\"Account\":\"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb\",\"date\":469441410,\"ledger_index\":508838,\"Sequence\":4264,\"SigningPubKey\":\"BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D4\"}"
```

<a name="account_make_payment"></a>

### Submitting a Payment

```haskell
> import Web.Stellar.Payment

> let paymentParams = defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) &
                                             secret .~ "..." &
                                             fromAccount .~ "..." &
                                             toAccount .~ "..."

> r <- makePayment "https://test.stellar.org:9002" paymentParams

> let result = fromJust r

> result ^. status
PaymentSuccess

> result ^. errorMessage
Nothing
