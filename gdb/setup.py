#!/usr/bin/env python

from distutils.core import setup

setup(name='ghc-gdb',
      version='1.0',
      description='Utilities for debugging GHC within GDB',
      author='Ben Gamari',
      author_email='ben@smart-cactus.org',
      url='https://www.github.com/bgamari/ghc-utils/',
      packages=['ghc_gdb'],
     )
