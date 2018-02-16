.PHONY: publish

publish: Dockerfile
	docker build -t charleso/beer-bot .
	docker push charleso/beer-bot
