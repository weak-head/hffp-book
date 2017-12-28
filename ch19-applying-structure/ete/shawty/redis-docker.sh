#!/bin/sh

sudo docker run -p 6379:6379 --name local_redis -d redis
