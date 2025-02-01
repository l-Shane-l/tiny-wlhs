# TODO

## Project Goal

A wayland compositor controllable and customizable in Haskell

## Features

The following is a list of features under development, ordered by complexity from simple to most complex:

### Copy Paste Support

- Support basic clipboard functionality through clipboard clients
- Implement support for wl-clipboard or similar
- Enable basic text copy/paste operations between applications

### Advanced Wayland Client Support

- Expand protocol support for more complex clients
- Add support for waybar and other advanced status bars
- Implement additional Wayland protocols as needed

### Multi Screen Support

- Complete multi-monitor implementation
- Support window movement between screens
- Handle different monitor configurations and resolutions
- Already partially implemented in the codebase

### Tiling Management

- Add support for multiple screen layouts
- Implement tiling configurations
- Enable cycling through different layout modes
- Support window positioning and sizing rules

### Locking Support

- Implement secure system locking mechanism
- Handle screen blanking and security
- Ensure proper session management during lock
- Most complex feature due to security requirements

## Code Quality

### Configuration and Documentation

- Keep documentation in sync with code changes
- Improve setup and configuration guides
- Ensure README accurately reflects current implementation
- Add more configuration examples

### Performance Optimization

- Profile and optimize FFI call performance
- Identify and address performance bottlenecks
- Improve critical path efficiency
- Consider compiler plugin for FFI profiling

### Testing Implementation

- Add testing frameworks for both Haskell and C components
- Implement initial unit tests
- Add test documentation
- Set up testing infrastructure

### Code Structure

- Refactor C code into manageable components
- Standardize FFI implementation
- Remove usage of Peek/Poke in .hsc files
- Add proper getters/setters in C code

### CI/CD Pipeline

- Implement GitHub Actions workflow
- Add build verification
- Set up automated testing
- Add code style checks
- Generate build artifacts

## Development Process

- Maintain consistent code quality
- Follow contribution guidelines
- Keep documentation up to date with changes
- Regular testing and performance monitoring
