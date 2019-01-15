#!/usr/bin/env python

from distutils.core import setup

setup(name='ben-mail',
      author='Ben Gamari',
      author_email='ben@smart-cactus.org',
      py_modules=['fetch_gitlab'],
      entry_points={
          'console_scripts': [
              'fetch-gitlab=fetch_gitlab:main',
          ]
      }
     )
