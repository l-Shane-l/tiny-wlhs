# Contributing to TinyWL-hs

We welcome contributions to the TinyWL-hs project! This is a Wayland compositor written in Haskell, and we're actively working on expanding its features and capabilities.

## Getting Started

1. Ensure you have the prerequisites installed:

   - Nix package manager
   - Git
   - Basic understanding of Wayland, Haskell, and C

2. Fork and clone the repository with submodules:
   ```bash
   git clone --recurse-submodules https://github.com/[your-username]/tiny-wlhs.git
   ```

## Areas for Contribution

Current priority areas for contribution include:

1. Copy/Paste Functionality

   - Implementing basic clipboard support
   - Integration with wl-clipboard

2. Multi-Monitor Support

   - Handling multiple displays
   - Window management across screens

3. Performance Optimization

   - FFI call profiling
   - Performance bottleneck identification
   - Optimization implementation

4. Testing

   - Setting up Haskell testing framework
   - Setting up C testing framework
   - Writing initial test cases

5. Documentation
   - Keeping documentation in sync with code changes
   - Improving setup guides
   - Adding configuration examples

## Development Process

1. Check existing issues and TODO.md for current tasks
2. Comment on an issue you want to work on
3. Fork the repository
4. Create a feature branch
5. Make your changes
6. Submit a pull request

## Code Guidelines

### Haskell Code

- Follow standard Haskell style conventions
- Use meaningful variable names
- Document exported functions with Haddock
- Keep functions focused and modular

### C Code

- Follow existing C code style in the project
- Use proper memory management
- Document functions and complex logic
- Avoid direct memory manipulation where possible

### FFI

- Prefer function calls over Peek/Poke
- Implement proper getters/setters in C
- Document FFI bindings clearly

## Pull Request Process

1. Ensure your code builds without errors
2. Update documentation if you're changing functionality
3. Update CHANGELOG.md with your changes
4. Submit a pull request with a clear description of changes

## Communication

- Use GitHub issues for bug reports and feature discussions
- For questions or suggestions, contact: shane@peregrinum.dev
- Check existing issues before creating new ones

## Project Structure

- `src/` - Haskell source code
- `c/` - C source code
- `Config.hs` - Main configuration
- `Setup.hs` - Setup functionality
- `Main.hs` - Application entry point

## License

By contributing, you agree that your contributions will be licensed under the project's license.

Thank you for contributing to TinyWL-hs
