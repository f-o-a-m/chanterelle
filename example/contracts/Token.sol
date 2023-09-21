// SPDX-License-Identifier: MIT

pragma solidity ^0.8.0;

import "openzeppelin-contracts/token/ERC20/ERC20.sol";
import "openzeppelin-contracts/access/Ownable.sol";

contract Token is ERC20, Ownable {
    
    constructor(uint256 initialSupply) ERC20("MyTokenName", "MYT") {
        _mint(msg.sender, initialSupply);
    }

    // This function allows the owner to mint more tokens.
    function mint(address to, uint256 amount) external onlyOwner {
        _mint(to, amount);
    }

    // This function allows the owner to burn tokens from any account. This is optional and might not be desired in most scenarios.
    function burn(address from, uint256 amount) external onlyOwner {
        _burn(from, amount);
    }
}
