# C/ACSL Features Implementation

This software implments a C/ACSL Language Server compliant with the Language Server Protocol (LSP).
It is a Frama-C plugin that aims to offer an enhanced coding experience with functionalities that cater to various development needs.


## LSP Features

- Go To Definition
- Go To Declaration
- Diagnostics

## Non LSP features
- Syntax Highlighting: Supports syntax highlighting for a C and ACSL programming languages
- Display CIL (preprocessed code by Frama-C)
- Display Callgraph
- Display Metrics
- Prove property (specific to Frama-C/WP plugin)
- Show proof obligations (specific to Frama-C/WP plugin)


## Installation

### Via opam
opam pin add .

### Manual installation
dune build
dune install


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
This project is licensed under the GPLv3 License - see the LICENSE file for details.

## Acknowledgments
This software has been initiated in 2024 by Djamila MOHAMED and Adel DJOUDI.