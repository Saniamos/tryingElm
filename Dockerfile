FROM node:latest

# elm
RUN npm install -g elm@0.18.0
RUN npm install -g elm-test

# make everything accessible
ENTRYPOINT ["elm"]