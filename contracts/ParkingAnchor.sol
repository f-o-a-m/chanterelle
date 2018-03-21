pragma solidity ^0.4.13;

/*
  The ParkingAnchor represents a physical device that has a fixed location, and can be user to
  validate a users request to pay for parking. It has two pieces of data, an authority and an anchorId,
  which can be used as an external uuid. It is registered to an authority, hence also to
  the authority's spatial registry. In order to accept payment from a user, the user must be
  registered and authorized to park in the zone in which the anchor lives.
*/

import "./CSC.sol";
import "./User.sol";
import "./ParkingAuthority.sol";

contract ParkingAnchor is Ownable, CSC {

  bytes32 public anchorId;
  ParkingAuthority public anchorAuthority;

  event PaymentAccepted(address anchor, address user);

  function ParkingAnchor(bytes8 _geohash, bytes32 _anchorId) public CSC(_geohash) Ownable() {  
    anchorId = _anchorId;
    anchorAuthority = ParkingAuthority(msg.sender);
  }
    
  function acceptPayment() public payable returns(bool) {
      bytes4 anchorZone = bytes4(geohash);
      User user = anchorAuthority.members(msg.sender);
      require(msg.value > 0);
      
      if (user.licensedZones(anchorZone) && user.pendingAnchor() == this) {
          PaymentAccepted(this, address(user));
          user.setLastCheckIn();
          return true;
      } else {
          revert();
      }
  }

  function transferBalanceToOwner() public onlyOwner() {
      owner.transfer(this.balance);
  }
}
