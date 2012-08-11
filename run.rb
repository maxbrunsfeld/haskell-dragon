#!/usr/bin/env ruby

# Compile script with ghc
# Name the executable such that it will be ignored by git
# Run it
src = ARGV[0]
exe = "#{src}.out"
system "ghc #{src} -o #{exe} && #{exe}"
