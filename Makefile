build: src/Main.elm
	elm make --optimize src/Main.elm --output public/haskle.js

.PHONY: deploy
deploy:
	AWS_PROFILE=clever aws s3 cp --acl public-read public/haskle.png s3://haskle.net/haskle.png --endpoint-url https://cellar-c2.services.clever-cloud.com
	AWS_PROFILE=clever aws s3 cp --acl public-read public/haskle.js s3://haskle.net/haskle.js --endpoint-url https://cellar-c2.services.clever-cloud.com
	AWS_PROFILE=clever aws s3 cp --acl public-read public/index.html s3://haskle.net/index.html --endpoint-url https://cellar-c2.services.clever-cloud.com
