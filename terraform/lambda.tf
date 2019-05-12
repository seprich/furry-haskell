
data aws_iam_policy_document lambda_policy {
  statement {
    actions = ["sts:AssumeRole"]
    principals {
      type        = "Service"
      identifiers = ["lambda.amazonaws.com"]
    }
  }
}

resource aws_iam_role iam_for_lambda {
  name               = "iam_for_lambda"
  assume_role_policy = data.aws_iam_policy_document.lambda_policy.json
}

resource aws_lambda_function furry_lambda {
  function_name    = "FurryHaskell"
  filename         = "../out/furry.zip"
  handler          = "does.not.matter"
  role             = aws_iam_role.iam_for_lambda.arn
  source_code_hash = filebase64sha256("../out/furry.zip")
  runtime          = "provided"
  memory_size      = 128

  depends_on = [
    aws_cloudwatch_log_group.furry_lambda,
    aws_iam_role_policy_attachment.lambda_logs
  ]
}

resource aws_cloudwatch_log_group furry_lambda {
  name              = "/aws/lambda/FurryHaskell"
  retention_in_days = 7
}


resource aws_iam_role_policy_attachment lambda_logs {
  role       = aws_iam_role.iam_for_lambda.name
  policy_arn = "arn:aws:iam::aws:policy/service-role/AWSLambdaBasicExecutionRole"
}
