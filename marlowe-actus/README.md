# Marlowe ACTUS: standardised financial contracts on Cardano

_marlowe-actus_ is a library to generate Marlowe contracts from ACTUS contract terms

## ACTUS - Algorithmic Contract Types Unified Standards

ACTUS is a foundation that defines the ACTUS taxonomy of financial contracts, see https://www.actusfrf.org/

The following contract types are implemented in Haskell and Marlowe.

### Amortizing loans

An amortizing loan is a loan that requires periodic payments where a payment consists of the interest payment and the principal.

#### Principal at maturity (PAM)

Principal at maturity only defines periodic interest payments, the full principal is due at maturity.
A simple illustrative example of such a contract is the following

```
principal: 10000
interest rate: 2% p.a.
annual interest payments
term: 10 years
```

This generates yearly cash flows with the interest payment for 10 years and the principal repayment at maturity:

|Year|Balance|Principal|Interest|Payment|
|----|-------|---------|--------|-------|
|1|10000|0|200|200|
|2|10000|0|200|200|
|3|10000|0|200|200|
|4|10000|0|200|200|
|5|10000|0|200|200|
|6|10000|0|200|200|
|7|10000|0|200|200|
|8|10000|0|200|200|
|9|10000|0|200|200|
|10|10000|10000|200|10200|

[ACTUS contract terms](test/Spec/Actus/ex_pam1.json)

#### Linear Amortizer (LAM)

Regular principal repayments over time, the interest payments decrease linearly.

```
principal: 10000
principal repayment: 1000 p.a.
interest rate: 2% p.a.
annual interest payments
term: 10 years
```

|Year|Balance|Principal|Interest|Payment|
|----|-------|---------|--------|-------|
|1|10000|1000|200|1200|
|2|9000|1000|180|1180|
|3|8000|1000|160|1160|
|4|7000|1000|140|1140|
|5|6000|1000|120|1120|
|6|5000|1000|100|1100|
|7|4000|1000|80|1080|
|8|3000|1000|60|1060|
|9|2000|1000|40|1040|
|10|1000|1000|20|1020|

[ACTUS contract terms](test/Spec/Actus/ex_lam1.json)

#### Negative Amortizer (NAM)

Negative amortization means that the payments per period are smaller than the interest, i.e. the balance of the loan increases over time.

```
principal: 10000
interest rate: 2% p.a.
annual interest payments
term: 10 years
```

|Year|Balance|Principal|Interest|Payment|
|----|-------|---------|--------|-------|
|1|10000|1000|200|1000|
|2|9200|1000|184|1000|
|3|8384|1000|168|1000|
|4|7552|1000|151|1000|
|5|6703|1000|134|1000|
|6|5837|1000|117|1000|
|7|4954|1000|99|1000|
|8|4053|1000|81|1000|
|9|3134|1000|63|1000|
|10|2196|1000|44|1000|

[ACTUS contract terms](test/Spec/Actus/ex_nam1.json)

#### Annuity (ANN)

The annuity amortization consists of regular payments of equal amounts over the lifetime of the loan.

```
principal: 10000
interest rate: 2% p.a.
annual interest payments
term: 10 years
```

|Year|Balance|Principal|Interest|Payment|
|----|-------|---------|--------|-------|
|1|10000|800|200|1000|
|2|9200|816|184|1000|
|3|8384|832|168|1000|
|4|7552|849|151|1000|
|5|6703|866|134|1000|
|6|5837|883|117|1000|
|7|4954|901|99|1000|
|8|4053|919|81|1000|
|9|3134|937|63|1000|
|10|2196|956|44|1000|

[ACTUS contract terms](test/Spec/Actus/ex_ann1.json)

#### Stock

A stock is a contract that pay dividends based on a schedule.

#### Option

An option is the right to buy (or sell) a specific underlying at a defined date for a defined price.

```
option type: Call
exercise type: European
underlying: XXX
strike: 80
price at purchase date: 10
purchase date: 2.1.2020
maturity date: 30.3.2020
```

|Date|Asset|Value|
|----|-----|-----|
|Maturity|Underlying|120|
|Maturity|Option|40|

[ACTUS contract terms](test/Spec/Actus/ex_optns1.json)

#### Future

A future is the obligation to buy (or sell) a specific underlying at a defined date for a defined price.

## Test cases

For the contract types mentioned above the implementation is tested with the test cases provided by ACTUS: https://github.com/actusfrf/actus-tests
