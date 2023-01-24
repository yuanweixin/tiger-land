# Package

version       = "0.1.0"
author        = "Wei Xin Yuan"
description   = "Tiger lang implementation in Nim"
license       = "MIT"
srcDir        = "src"
bin           = @["tiger"]


# Dependencies

requires "nim >= 1.6.8"
requires "https://github.com/yuanweixin/lexim >= 1.0.0"
requires "https://github.com/yuanweixin/nimyacc >= 1.0.0"