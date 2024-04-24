# BIP-0122 handler
[BIP-0122](https://github.com/bitcoin/bips/blob/master/bip-0122.mediawiki) handler for blockchain URIs (console application)  
Copyright (c) 2020 Erik Jan Meijer  
Written in [Lazarus, the professional Free Pascal RAD IDE](https://www.lazarus-ide.org/)  
Currently available for Windows 7+
## Purpose
The purpose of this application is to be registered as the system handler for blockchain: URIs. You then configure your desired block explorer(s). This way, you can for instance use the feature 'View on block explorer' of [Electrum](https://www.electrum.org/) by setting the *Online Block Explorer* to 'system default'.

## Installation
Download the Windows installer from the [latest release at GitHub](https://github.com/Eegee/BIP-0122-handler/releases/latest) and run it.
Follow the steps of the installer.
## Usage
The blockchain protocol handler needs to be set up on your system before you can open blockchain: URIs.
The installer asks to choose your desired block explorer(s) as a last step in a new Command Prompt window. If you've skipped this step, you can manually set the block explorers in a Command Prompt with the following command:

    bip0122handler.exe -s "Blockstream, ""Blockchain Reader"""
In this example *Blockstream* and *Blockchain Reader* are chosen as the block explorers. You can choose more than one explorer by separating their names with a comma.  
  
Use

    bip0122handler.exe -h
for help.
## Files
The application file `block-explorers.json`, included in the application installation directory, contains the list of block explorers, in JSON format.  
If you wish, it's possible to add your own custom explorers by creating a JSON file in the same structure and with the same name in `%LOCALAPPDATA%\bip0122handler\`.
Entries you add that have a new unique name file will be added to the list. Entries you add that have the same name as in the application file will overrule those from the application file.
