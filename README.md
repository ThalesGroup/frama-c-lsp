# ACSl LSP

This repository contains both the server and client software that implement the Language Server Protocol (LSP) for C/ACSL language. 
Refer to README files in each subdirectory for installation guidelines.

The client part is inspired by https://github.com/Microsoft/vscode-extension-samples/tree/main/lsp-sample

The server part is a Frama-C plugin called "lsp" that has two usage modes:
- handler mode: ensures sustainable exchange of TCP/IP sockets between the server and the client.
- feature mode: ensures background Frama-C process execution with parameters required for each requested feature.

## Configuration
This software is compatible with Frama-C v29.0/v30.0 and Linux ubuntu 22.04 LTS.
Ongoing development aims to adding new features and offering compatibiliy with more recent versions of Frama-C.

## Contributing
We welcome contributions to both the server and client parts of this solution !
If you would like to contribute, follow these steps:

- Clone or fork the repository.
- Create a new branch (git checkout -b feature/your-feature).
- Make your changes.
- Commit your changes (git commit -am 'Add new feature').
- Push to the branch (git push origin feature/your-feature).
- Open a pull request.
- Please ensure your code adheres to the project's coding standards and includes appropriate tests if necessary.

## License
This project is under the LGPLv3 License - see the LICENSE file for details.

## Acknowledgments
This software has been initiated in 2024 by Djamila MOHAMED and Adel DJOUDI.
