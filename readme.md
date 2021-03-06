### Stellar api bindings for Haskell

__In development, not safe for use really__

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
* [Viewing available currencies](#account_currencies)
* [Viewing trust lines for an account](#account_trust_lines)
* [Viewing account transations](#account_transactions)
* [Setting trust](#account_set_trust)
* [Submitting offers](#account_make_offer)
* [Submitting payments](#account_make_payment)
* [Signing requests](#sign_request)

### See also

* [Running tests](#tests)

<a name="ping"></a>

### Pinging stellard

```haskell
> import Web.Stellar.Request

> pingStellar (Endpoint "https://test.stellar.org:9002")
Right PingSuccess

> pingStellar (Endpoint "https://google.com")
Left PingFailure
```

<a name="account_info"></a>

### Viewing Account Information

For further details on what these fields mean, consult the [Stellar
documentation](https://www.stellar.org/api/#api-account_info).

```haskell
> import Web.Stellar.Account
> import Data.Maybe
> import Control.Lens

> r <- fetchAccount (Endpoint "https://test.stellar.org:9002") (AccountID "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb")

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

<a name="account_currencies"></a>

### Viewing Available Currencies

```haskell
> import Web.Stellar.AccountCurrency

> r <- fetchCurrencies (Endpoint "https://test.stellar.org:9002") (AccountID "gM4Fpv2QuHY4knJsQyYGKEHFGw3eMBwc1U")

> let c = fromJust r

> c ^. send
["BOO","MOZ","USD"]

> c ^. receive
["MOZ"]
```

<a name="account_trust_lines"></a>

### Viewing Account Lines

[API documentation for account lines](https://www.stellar.org/api/#api-account_lines)

```haskell
> import Web.Stellar.AccountLine

> r <- fetchAccountLines (Endpoint "https://test.stellar.org:9002") (AccountID "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb")

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

Here, earliestLedger and currenctLedger are used to set the boundaries of transaction history. 0 is the earliest ledger to fetch
from, -1 is the latest to fetch from (-1 signifying the current ledger). See the
[docs](https://www.stellar.org/api/#api-account_tx) for more.


```haskell
> import Web.Stellar.Transaction

> r <- fetchTransactions (Endpoint "https://test.stellar.org:9002") (AccountID "ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb") earliestLedger currentLedger

> let transaction = (Prelude.head.fromJust) r

> transaction ^. transactionAccount
"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb"

> transaction ^. destination
Just "gHJPw9kW8v4BsUyDnBR8ZHWo8aEkhUMeAq"

> transaction ^. signingPubKey
"BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D4"

> transaction ^. transactionType
Payment

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

> transaction ^? currency
Nothing

> transaction ^. rawTransaction
"{\"TransactionType\":\"Payment\",\"Amount\":\"1000000000\",\"Destination\":\"gHJPw9kW8v4BsUyDnBR8ZHWo8aEkhUMeAq\",\"Flags\":2147483648,\"hash\":\"B5B8DD8668321C987C18F6131269455AA4CD6FD9E8AE6E5C4EA26BA9EDF7F487\",\"inLedger\":508838,\"TxnSignature\":\"82F10F2144AA2D10B888F5E7C69D9669CD9E126EFDE3ED8704EF5FB2C70ABE79FCE787B62D8C49001527B08E0655F9A624D5D33A3CA96A8ABAE86353811C5607\",\"Fee\":\"10\",\"Account\":\"ganVp9o5emfzpwrG5QVUXqMv8AgLcdvySb\",\"date\":469441410,\"ledger_index\":508838,\"Sequence\":4264,\"SigningPubKey\":\"BE3900393891A2A2244E28A82C43BA94CA94DD6BFE36D523576A22BFF86055D4\"}"
```

<a name="account_set_trust"></a>

### Setting Trust

```haskell
> import Web.Stellar.TrustLine

-- Currency, Issuer, Amount triple
> let trustAmount = WithCurrency (CurrencyCode "USD") (Issuer "gBAde4mkDijZatAdNhBzCsuC7GP4MzhA3B") 1

> let trustParams = defaultTrustSetParams & paymentAmount .~ (WithMicroStellars 1) &
                                            secret .~ (Secret "...") &
                                            account .~ (AccountID "...") &
                                            flags .~ (Flags 131072) &
                                            sequence .~ (Sequence 123)

> r <- setTrust (Endpoint "https://test.stellar.org:9002") trustParams

> let result = fromJust r

> result ^. status
SubmissionSuccess
```

<a name="account_make_offer"></a>

### Submitting an Offer

```haskell
> import Web.Stellar.Offer

> let gets = WithCurrency (CurrencyCode "USD") (Issuer "...") 1500

> let pays = WithCurrency (CurrencyCode "BTC") (Issuer "...") 2.5

> let offerParams = defaultOfferParams & account .~ (AccountID "...") &
                                         takerGets .~ gets &
                                         takerPays .~ pays &
                                         sequence .~ (Sequence 123) &
                                         secret .~ (Secret "...")

> r <- offerCreate (Endpoint "https://test.stellar.org:9002") offerParams

> let result = fromJust r

> result ^. status
SubmissionSuccess

> result ^. errorMessage
Nothing
```

<a name="account_make_payment"></a>

### Submitting a Payment

```haskell
> import Web.Stellar.Payment

> let paymentParams = defaultPaymentParams & paymentAmount .~ (WithMicroStellars 1) &
                                             secret .~ "..." &
                                             fromAccount .~ "..." &
                                             toAccount .~ "..." &
                                             sequence .~ 123

> r <- makePayment (Endpoint "https://test.stellar.org:9002") paymentParams

> let result = fromJust r

> result ^. status
SubmissionSuccess

> result ^. errorMessage
Nothing
```

<a name="sign_request"></a>

### Signing Requests

It's not great to pass your secret to untrusted servers. The state of local signing with `ripple-lib` and `stellar-lib` is not amazing though - unfortunately the executable `rsign.js` is buggy, and changes frequently between versions. I have been using a local `stellard` instance to sign my requests, and this seems to work OK. I run it in standalone (`-a`) mode which is much lighter on resources than running a full `stellard` instance. Currently `stellar-haskell` supports signing against a separate instance for payments; and I'm working on adding support for other submission types.

```haskell
> import Web.Stellar.Signing

> r <- signRequest (Endpoint "http://localhost:5005") (toSignRequest paymentParams)

> let res = fromJust r
> res ^. blob
"12000022800000002400...."

> finalResponse <- makeSignedRequest (Endpoint "https://test.stellar.org:9002") res

> (fromJust finalResponse) ^. status
SubmissionSuccess
```

<a name="tests"></a>

### Running tests

Requires an active `stellard` instance running at `localhost:5005`:

```shell
> cabal install --enable-tests

> cabal test
```
