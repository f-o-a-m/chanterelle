pragma solidity ^0.4.13;

/*
  A User is the proxy contract for an ethereum account that wants to use the ParkingAuthority's
  services. If they sign up through the authority (which is the only way), they will be given
  exclusive ownership over their account.

  They use this account to pay for parking in the zones they are allowed. If they want
  to modify their parking priveledges, they must do this through the zone authority. 
*/

import "zeppelin-solidity/contracts/ownership/Ownable.sol";

import "./ParkingAnchor.sol";
import "./ParkingAuthority.sol";

contract User is Ownable {
    
    ParkingAnchor public lastCheckIn;
    uint public lastCheckInBlock;
    ParkingAnchor public pendingAnchor;
    ParkingAuthority public parkingAuthority;
    mapping(bytes4 => bool) public licensedZones;

    event CheckIn(address user, address anchor);

    function User() public Ownable {
      parkingAuthority = ParkingAuthority(msg.sender);
    }
    
    function setPendingAnchor(ParkingAnchor _anchor) internal onlyOwner() {
        pendingAnchor = _anchor;
    }

    function payForParking(ParkingAnchor _anchor) public payable onlyOwner() {
        setPendingAnchor(_anchor);
        _anchor.acceptPayment.value(msg.value)();
    }
    
    function setLastCheckIn() public {
        ParkingAnchor anchor = ParkingAnchor(msg.sender);
        require(anchor == pendingAnchor);
        lastCheckIn = anchor;
        lastCheckInBlock = block.number;
        CheckIn(this, address(anchor));
        pendingAnchor = ParkingAnchor(0);
    }

    function addZone(bytes4 _zone) public {
      require(msg.sender == address(parkingAuthority));
      licensedZones[_zone] = true;
    }

    function requestZone(bytes4 _zone) public onlyOwner {
      parkingAuthority.addZone(_zone);
    }
}
