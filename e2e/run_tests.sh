#cucumber tag
tag=$1

yarn run cucumber --profile $tag || yarn run postcucumber