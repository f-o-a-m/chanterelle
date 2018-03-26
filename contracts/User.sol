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
    event ZoneGranted(bytes4 zone);

    modifier callerIsParkingAuthority() {
        ParkingAuthority authority = ParkingAuthority(msg.sender);
        require(parkingAuthority == authority);
        _;
    }

    // make sure the sender of this message is the valid pendingAnchor.
    modifier callerIsPendingAnchor() {
        require(parkingAuthority.anchors(msg.sender));
        ParkingAnchor anchor = ParkingAnchor(msg.sender);
        require(anchor == pendingAnchor);
        _;
    } 

    // A user is created by a ParkingAuthorty.
    function User() public Ownable {
        parkingAuthority = ParkingAuthority(msg.sender);
    }

    // Set the pending anchor to indicate interest in using a ParkingAnchor
    function setPendingAnchor(ParkingAnchor _anchor) internal onlyOwner() {
        pendingAnchor = _anchor;
    }

    // Pay a valid parking anchor for parking, assuming the user has access to their zone.
    function payForParking(ParkingAnchor _anchor) public payable onlyOwner() {
        require(parkingAuthority.anchors(address(_anchor)));
        setPendingAnchor(_anchor);
        _anchor.acceptPayment.value(msg.value)();
    }

    // the pendingAnchor will call this function to complete the CheckIn and to set the
    // pendingAnchor to null.
    function setLastCheckIn() public callerIsPendingAnchor() {
        ParkingAnchor anchor = ParkingAnchor(msg.sender);
        lastCheckIn = anchor;
        lastCheckInBlock = block.number;
        CheckIn(this, msg.sender);
        pendingAnchor = ParkingAnchor(0);
    }

    // The parking authority calls this function to modify the set of licensedZones.
    function addZone(bytes4 _zone) public callerIsParkingAuthority() {
        licensedZones[_zone] = true;
        ZoneGranted(_zone);
    }

    // the owner of the User contract can propose to the ParkingAuthority to add
    // a zone to their set of licensedZones.
    function requestZone(bytes4 _zone) public onlyOwner {
        parkingAuthority.addZone(_zone);
    }
}
