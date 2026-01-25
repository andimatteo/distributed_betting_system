package it.unipi.distributed_betting_system.security;

import io.jsonwebtoken.Jwts;
import io.jsonwebtoken.SignatureAlgorithm;
import io.jsonwebtoken.security.Keys;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import java.nio.charset.StandardCharsets;
import java.time.Instant;
import java.util.Date;
import java.util.Map;

@Service
public class JwtService {
    private final byte[] signingKey;
    private final long expirationSeconds;

    public JwtService(
            @Value("${app.jwt.secret}") String secret,
            @Value("${app.jwt.expiration-seconds}") long expirationSeconds
    ) {
        this.signingKey = secret.getBytes(StandardCharsets.UTF_8);
        this.expirationSeconds = expirationSeconds;
    }

    public String generateToken(long id, boolean isAdmin) {
        Instant now = Instant.now();
        Instant expiry = now.plusSeconds(expirationSeconds);
        // TODO: see if put expiration manually or not
        return Jwts.builder()
                .claims(Map.of(
                        "id", id,
                        "isAdmin", isAdmin
                ))
                .issuedAt(Date.from(now))
                .expiration(Date.from(expiry))
                .signWith(Keys.hmacShaKeyFor(signingKey), SignatureAlgorithm.HS256)
                .compact();
    }

    public long expiryEpochSeconds() {
        return Instant.now().plusSeconds(expirationSeconds).getEpochSecond();
    }
}
