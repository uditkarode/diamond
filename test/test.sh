#!/bin/dash
sudo docker run -it --entrypoint /bin/bash --network "host" diamondtest
echo 'y' | sudo docker container prune

