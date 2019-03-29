#!/usr/bin/env python

from distutils.core import setup

setup(name='gitlab-utils',
      author='Ben Gamari',
      author_email='ben@smart-cactus.org',
      packages=['gitlab_utils'],
      entry_points={
          'console_scripts': [
              'gitlab-labels=gitlab_utils.labels:main',
          ]
      }
     )
