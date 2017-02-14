'use strict;'

var Docker = require('gulp-docker')
  , gulp = require('gulp')
  ;

new Docker(gulp, {
  reactor: 
  { name: 'elm'
  , dockerfile: 'Dockerfile'
  , build: '-t'
  , run: '-it --rm -w "/code" -u 1000 elm reactor -a 0.0.0.0'
  , env: { HOME: '/tmp' }
  , volumes: '$(pwd):/code'
  , ports: ['8000:8000']
  , repo: ''
  , tags: ['development']
  }
});

// docker run   -it --rm -v "$(pwd):/code"   -w "/code" -e "HOME=/tmp"   -p 8000:8000 -u 1000  elm reactor -a 0.0.0.0