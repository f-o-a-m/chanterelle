pragma solidity ^0.4.13;

import "./CSC.sol";

/*
  A CSR is a registry (a lookup table), which maps a Crypto Spatial Cooridinate
  (CSC) to its contract address. If you would like to register a CSC, use the
  CSC's register method against the registery. The registry is in charge of making
  sure this request makes sense before commiting the entry.
*/

contract CSR {
    function registry(bytes12 csc) public returns(address);
    function register(CSC csc) public;
}
