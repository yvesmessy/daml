package com.digitalasset.ledger.client.grpcHeaders

sealed abstract class AuthorizationConfig extends Product with Serializable

object AuthorizationConfig {
  final case class FileAccessToken(filePath: String) extends AuthorizationConfig
  final case class LiveAccessToken(uri: String) extends AuthorizationConfig
}
