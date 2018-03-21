pragma solidity ^0.4.13;

import "./CSR.sol";
import "./Ownable.sol";

/*
  ParkingCSR is the spatial registry contract for the ParkingAuthority, and is owned
  by it. It implements the minimum CSR methods in a straightforward way.
*/

contract ParkingCSR is Ownable, CSR {
    
  mapping(bytes12 => address) CSCRegistry;

  function ParkingCSR() public Ownable() {
      
  }

  function registry(bytes12 csc) public returns(address) {
      return CSCRegistry[csc];
  }

  function register(CSC newCsc) public onlyOwner() {
    bytes12 csc = newCsc.csc();
    if (CSCRegistry[csc] == 0) {
      address cscAddr = address(newCsc);
      bytes8 geohash = newCsc.geohash();
      bytes12 computedCSC = SpatialUtils.computeCSC(geohash, cscAddr);
      require(computedCSC == csc);
      CSCRegistry[csc] = cscAddr;
    }
  }
}