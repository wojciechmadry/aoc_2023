help:
	@echo make con - \(Start and\) Connect to the container
	@echo make run - Run container
	@echo make build - Build container

con:
	docker start scala
	docker exec -it scala /bin/bash

build:
	docker build -t scala -f Dockerfile.scala .

run:
	docker run -itd --volume="${PWD}:/home/${USER}/aoe:rw" --name scala scala

