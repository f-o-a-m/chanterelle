// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

import "openzeppelin-contracts/token/ERC20/IERC20.sol";

contract SimplePaidStorage {
    IERC20 private _token;
    address private _owner;
    uint public count = 0;

    event CountUpdated(uint newCount);

    constructor(address tokenAddress) {
        _token = IERC20(tokenAddress);
        _owner = msg.sender;
    }

    modifier onlyOwner() {
        require(msg.sender == _owner, "Not contract owner");
        _;
    }

    function updateCount(uint _newCount) external {
        require(
            _token.balanceOf(msg.sender) >= 1,
            "Insufficient tokens to update count"
        );
        require(
            _token.allowance(msg.sender, address(this)) >= 1,
            "Token allowance not set for contract"
        );

        _token.transferFrom(msg.sender, address(this), 1);

        count = _newCount;
        emit CountUpdated(_newCount);
    }

    // withdraw tokens to owner, pay the sender a fee
    function withdrawTokens(uint amount) public onlyOwner {
        _token.transfer(_owner, amount);
    }
}
