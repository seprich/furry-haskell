# furry-haskell

Haskell lambda project

## Build

```sh
$ ./buildBuildBox
```
```sh
$ ./runBuildBox
workdir]$ stack --local-bin-path="./out" build --copy-bins
workdir]$ mv out/furry-haskell-exe out/bootstrap
workdir]$ exit
$ cd out/ && zip furry.zip bootstrap
```

## Deploy
```sh
$ cd terraform
$ terraform init
$ terraform apply
```

## Test
```
$ aws lambda invoke --region=eu-north-1 --function-name=FurryHaskell --payload '{"test": "this", "numbers": [1, 2, 3, 4, 5]}' output.txt
```
Output contains the echoed payload
