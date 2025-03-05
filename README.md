# ACSl LSP

This repository contains both the server and client software that implement the Language Server Protocol (LSP) for C/ACSL language. 
Refer to README files in each subdirectory for installation guidelines.

The client part is inspired from https://github.com/Microsoft/vscode-extension-samples/tree/main/lsp-sample

The server part is a Frama-C plugin called "LSP" that has two usage modes:
- handler mode: ensures sustainable exchange of TCP/IP sockets between the server and the client.
- feature mode: ensures background frama-c process execution with parameters required for requested feature.


## Contributing
We welcome contributions from the community! If you would like to contribute, follow these steps:

Fork the repository.
Create a new branch (git checkout -b feature/your-feature).
Make your changes.
Commit your changes (git commit -am 'Add new feature').
Push to the branch (git push origin feature/your-feature).
Open a pull request.
Please ensure your code adheres to the project's coding standards and includes appropriate tests if necessary.

## License
This project is licensed under the LGPLv3 License - see the LICENSE file for details.

## Acknowledgments
This software has been initiated in 2024 by Djamila MOHAMED and Adel DJOUDI.