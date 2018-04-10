# chanterelle

[![Build Status](https://travis-ci.org/f-o-a-m/chanterelle.svg?branch=master)](https://travis-ci.org/f-o-a-m/chanterelle)

<img src=https://github.com/f-o-a-m/purescript-web3/blob/master/purescript-web3-logo.png width="75">

_a build tool with example application_

## Parking DAO

The Parking DAO is a set of smart contracts that encapsulate a `User`, representing an account which is granted permission to park in certains geographical zones, and a `ParkingAnchor`, representing an account which has the ability to accept payment for parking in certain zones. These accounts are deployed by a central authority called the `ParkingAuthority`, which is a governing contract in charge of account management. The `ParkingAuthority` also contains the logic for altering account permissions.

For more information about how to use the contracts, see [this README](https://github.com/f-o-a-m/chanterelle/blob/master/sequence-diagrams/README.md), or look at the contracts in the `/contracts` directory. You can find the tests in `/test` that verify their behaviour.
