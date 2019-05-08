BOOTSTRAP_ACCOUNTS = [
    ["edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
     "4000000000000"],
    ["edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
     "4000000000000"],
    ["edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV",
     "4000000000000"],
    ["edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU",
     "4000000000000"],
    ["edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n",
     "4000000000000"]
]

COMMITMENTS = [
    ["btz1bRL4X5BWo2Fj4EsBdUwexXqgTf75uf1qa", "23932454669343"],
    ["btz1SxjV1syBgftgKy721czKi3arVkVwYUFSv", "72954577464032"],
    ["btz1LtoNCjiW23txBTenALaf5H6NKF1L3c1gw", "217487035428348"],
    ["btz1SUd3mMhEBcWudrn8u361MVAec4WYCcFoy", "4092742372031"],
    ["btz1MvBXf4orko1tsGmzkjLbpYSgnwUjEe81r", "17590039016550"],
    ["btz1LoDZ3zsjgG3k3cqTpUMc9bsXbchu9qMXT", "26322312350555"],
    ["btz1RMfq456hFV5AeDiZcQuZhoMv2dMpb9hpP", "244951387881443"],
    ["btz1Y9roTh4A7PsMBkp8AgdVFrqUDNaBE59y1", "80065050465525"],
    ["btz1Q1N2ePwhVw5ED3aaRVek6EBzYs1GDkSVD", "3569618927693"],
    ["btz1VFFVsVMYHd5WfaDTAt92BeQYGK8Ri4eLy", "9034781424478"]
]

PARAMETERS = {
    "bootstrap_accounts": BOOTSTRAP_ACCOUNTS,
    "commitments": COMMITMENTS,
    "time_between_blocks": ["1", "0"],
    "blocks_per_cycle": 128,
    "blocks_per_roll_snapshot": 32,
    "blocks_per_voting_period": 16,
    "preserved_cycles": 1,
    "proof_of_work_threshold": "-1"
}

GENESIS_SK = "edsk31vznjHSSpGExDMHYASz45VZqXN4DPxvsa4hAyY8dHM28cZzp6"


GENESIS_PK = "edpkuSLWfVU1Vq7Jg9FucPyKmma6otcMHac9zG4oU1KMHSTBpJuGQ2"


IDENTITIES = {
    'bootstrap1': {
        'identity': "tz1KqTpEZ7Yob7QbPE4Hy4Wo8fHG8LhKxZSx",
        'public': "edpkuBknW28nW72KG6RoHtYW7p12T6GKc7nAbwYX5m8Wd9sDVC9yav",
        'secret': ("unencrypted:"
                   "edsk3gUfUPyBSfrS9CCgmCiQsTCHGkviBDusMxDJstFtojtc1zcpsh")
    },
    'bootstrap2': {
        'identity': "tz1gjaF81ZRRvdzjobyfVNsAeSC6PScjfQwN",
        'public': "edpktzNbDAUjUk697W7gYg2CRuBQjyPxbEg8dLccYYwKSKvkPvjtV9",
        'secret': ("unencrypted:"
                   "edsk39qAm1fiMjgmPkw1EgQYkMzkJezLNewd7PLNHTkr6w9XA2zdfo")
    },
    'bootstrap3': {
        'identity': "tz1faswCTDciRzE4oJ9jn2Vm2dvjeyA9fUzU",
        'public': "edpkuTXkJDGcFd5nh6VvMz8phXxU3Bi7h6hqgywNFi1vZTfQNnS1RV",
        'secret': ("unencrypted:"
                   "edsk4ArLQgBTLWG5FJmnGnT689VKoqhXwmDPBuGx3z4cvwU9MmrPZZ")
    },
    'bootstrap4': {
        'identity': "tz1b7tUupMgCNw2cCLpKTkSD1NZzB5TkP2sv",
        'public': "edpkuFrRoDSEbJYgxRtLx2ps82UdaYc1WwfS9sE11yhauZt5DgCHbU",
        'secret': ("unencrypted:"
                   "edsk2uqQB9AY4FvioK2YMdfmyMrer5R8mGFyuaLLFfSRo8EoyNdht3")
    },
    'bootstrap5': {
        'identity': "tz1ddb9NMYHZi5UzPdzTZMYQQZoMub195zgv",
        'public': "edpkv8EUUH68jmo3f7Um5PezmfGrRF24gnfLpH3sVNwJnV5bVCxL2n",
        'secret': ("unencrypted:"
                   "edsk4QLrcijEffxV31gGdN2HU7UpyJjA8drFoNcmnB28n89YjPNRFm")
    },
    'activator': {
        'secret': "unencrypted:" + GENESIS_SK
    }
}


IDENTITIES_SHORT = {
    'activator': {
        'secret': "unencrypted:" + GENESIS_SK
    }
}


ALPHA = "ProtoALphaALphaALphaALphaALphaALphaALphaALphaDdp3zK"
ALPHA_DEAMON = "alpha"  # tezos-baker-alpha


TEZOS_CRT = """
Certificate:
    Data:
        Version: 3 (0x2)
        Serial Number: 1 (0x1)
    Signature Algorithm: sha256WithRSAEncryption
        Issuer: CN=Easy-RSA CA
        Validity
            Not Before: Mar 30 13:07:24 2018 GMT
            Not After : Mar 27 13:07:24 2028 GMT
        Subject: CN=tezos
        Subject Public Key Info:
            Public Key Algorithm: rsaEncryption
                Public-Key: (2048 bit)
                Modulus:
                    00:d3:61:ba:81:6a:0d:8f:0b:6f:84:65:ca:73:b5:
                    c6:2d:89:8e:83:90:9e:2c:e1:16:5f:2c:9d:44:00:
                    25:dd:a2:73:dc:41:06:81:fb:a1:0c:e9:17:db:63:
                    6b:c2:46:63:bc:31:4c:bc:76:50:a0:79:15:de:4a:
                    98:d1:eb:a3:d1:a9:4c:db:32:3e:05:23:be:60:b7:
                    5c:d1:4f:ec:fe:6d:a3:5f:75:0e:8d:e7:c5:d1:48:
                    6f:29:84:0a:cc:52:91:8b:8a:67:65:88:82:1a:a7:
                    31:6c:5c:00:1c:53:0e:fb:98:81:c7:5d:20:e8:72:
                    15:f1:53:e1:a8:e6:45:92:25:6b:a3:f6:67:da:63:
                    9f:fd:35:f6:54:04:c1:10:50:e9:5d:95:e3:12:7f:
                    e1:8f:bc:6c:65:48:f6:0c:eb:9e:d1:cb:30:1f:da:
                    ff:a2:d5:5d:bb:de:e5:df:b8:52:f3:70:6c:2d:8a:
                    e9:bb:85:7f:33:14:bc:fa:1e:c5:c4:b3:9f:f3:10:
                    a3:1c:00:f6:8f:84:ae:a3:a3:08:ae:b8:38:41:0a:
                    a7:84:88:bf:9d:e3:0d:42:51:75:dd:b2:5c:8b:9c:
                    fa:82:ff:0d:bd:6f:f7:c3:b5:e4:49:3a:5c:8c:cc:
                    7f:1c:80:7b:c1:47:ad:2c:fe:44:f1:fc:93:0e:ac:
                    4f:27
                Exponent: 65537 (0x10001)
        X509v3 extensions:
            X509v3 Basic Constraints:
                CA:FALSE
            X509v3 Subject Key Identifier:
                B4:C2:AB:C3:F6:64:80:94:43:46:7F:40:25:E4:D1:CF:01:33:44:DA
            X509v3 Authority Key Identifier:
                keyid:5E:27:08:3B:81:1D:FA:05:CC:D3:94:D4:2B:9B:92:5B:3B:F9:EA:A1
                DirName:/CN=Easy-RSA CA
                serial:D5:46:5A:8E:8B:18:BD:2B

            X509v3 Extended Key Usage:
                TLS Web Server Authentication
            X509v3 Key Usage:
                Digital Signature, Key Encipherment
            X509v3 Subject Alternative Name:
                DNS:tezos
    Signature Algorithm: sha256WithRSAEncryption
         2f:23:1a:9e:42:72:2b:57:ec:26:04:a2:a0:22:f3:31:0e:12:
         c4:46:92:95:b6:c7:44:bf:ab:5b:5b:15:c3:69:a3:48:79:be:
         f9:09:aa:42:8c:8a:83:6a:55:68:b7:6c:02:b4:1a:d4:98:52:
         b1:2e:bf:6c:3f:da:ef:93:e0:c8:69:fd:b7:dd:f7:42:65:e1:
         66:ab:99:c2:d7:81:62:e2:e9:63:98:8a:24:9b:34:da:8a:82:
         03:00:08:29:00:3f:18:cd:94:00:a7:22:0c:25:be:fa:74:64:
         ea:45:1f:62:e4:bd:f6:88:42:35:ca:7e:e4:a1:5f:a9:94:6d:
         4e:80:38:7b:3c:65:41:c4:e3:bc:40:de:50:b6:61:8c:ae:3a:
         de:d9:1e:af:e9:59:e3:c2:b2:5f:47:09:83:66:3c:d7:e5:4f:
         ec:27:8c:90:69:1d:6a:95:3e:2f:bf:89:95:58:ae:25:6d:90:
         bd:ce:41:63:91:58:e3:16:f9:08:70:c5:c1:5f:5d:f7:0d:a5:
         77:e5:a3:84:82:53:bf:30:6a:10:df:1c:b1:1f:81:c8:e0:c7:
         48:4d:74:47:21:48:3a:8a:80:f9:3c:43:c1:2c:0e:a4:40:51:
         b7:f3:b7:27:98:ab:23:cb:b1:05:67:59:ab:cf:23:f8:1b:9f:
         61:0d:8b:5e
-----BEGIN CERTIFICATE-----
MIIDSzCCAjOgAwIBAgIBATANBgkqhkiG9w0BAQsFADAWMRQwEgYDVQQDDAtFYXN5
LVJTQSBDQTAeFw0xODAzMzAxMzA3MjRaFw0yODAzMjcxMzA3MjRaMBAxDjAMBgNV
BAMMBXRlem9zMIIBIjANBgkqhkiG9w0BAQEFAAOCAQ8AMIIBCgKCAQEA02G6gWoN
jwtvhGXKc7XGLYmOg5CeLOEWXyydRAAl3aJz3EEGgfuhDOkX22NrwkZjvDFMvHZQ
oHkV3kqY0euj0alM2zI+BSO+YLdc0U/s/m2jX3UOjefF0UhvKYQKzFKRi4pnZYiC
GqcxbFwAHFMO+5iBx10g6HIV8VPhqOZFkiVro/Zn2mOf/TX2VATBEFDpXZXjEn/h
j7xsZUj2DOue0cswH9r/otVdu97l37hS83BsLYrpu4V/MxS8+h7FxLOf8xCjHAD2
j4Suo6MIrrg4QQqnhIi/neMNQlF13bJci5z6gv8NvW/3w7XkSTpcjMx/HIB7wUet
LP5E8fyTDqxPJwIDAQABo4GpMIGmMAkGA1UdEwQCMAAwHQYDVR0OBBYEFLTCq8P2
ZICUQ0Z/QCXk0c8BM0TaMEYGA1UdIwQ/MD2AFF4nCDuBHfoFzNOU1Cubkls7+eqh
oRqkGDAWMRQwEgYDVQQDDAtFYXN5LVJTQSBDQYIJANVGWo6LGL0rMBMGA1UdJQQM
MAoGCCsGAQUFBwMBMAsGA1UdDwQEAwIFoDAQBgNVHREECTAHggV0ZXpvczANBgkq
hkiG9w0BAQsFAAOCAQEALyMankJyK1fsJgSioCLzMQ4SxEaSlbbHRL+rW1sVw2mj
SHm++QmqQoyKg2pVaLdsArQa1JhSsS6/bD/a75PgyGn9t933QmXhZquZwteBYuLp
Y5iKJJs02oqCAwAIKQA/GM2UAKciDCW++nRk6kUfYuS99ohCNcp+5KFfqZRtToA4
ezxlQcTjvEDeULZhjK463tker+lZ48KyX0cJg2Y81+VP7CeMkGkdapU+L7+JlViu
JW2Qvc5BY5FY4xb5CHDFwV9d9w2ld+WjhIJTvzBqEN8csR+ByODHSE10RyFIOoqA
+TxDwSwOpEBRt/O3J5irI8uxBWdZq88j+BufYQ2LXg==
-----END CERTIFICATE-----
"""


TEZOS_KEY = """
-----BEGIN PRIVATE KEY-----
MIIEvgIBADANBgkqhkiG9w0BAQEFAASCBKgwggSkAgEAAoIBAQDTYbqBag2PC2+E
ZcpztcYtiY6DkJ4s4RZfLJ1EACXdonPcQQaB+6EM6RfbY2vCRmO8MUy8dlCgeRXe
SpjR66PRqUzbMj4FI75gt1zRT+z+baNfdQ6N58XRSG8phArMUpGLimdliIIapzFs
XAAcUw77mIHHXSDochXxU+Go5kWSJWuj9mfaY5/9NfZUBMEQUOldleMSf+GPvGxl
SPYM657RyzAf2v+i1V273uXfuFLzcGwtium7hX8zFLz6HsXEs5/zEKMcAPaPhK6j
owiuuDhBCqeEiL+d4w1CUXXdslyLnPqC/w29b/fDteRJOlyMzH8cgHvBR60s/kTx
/JMOrE8nAgMBAAECggEBAKjMC9E4TSeDbEP/vRF1gJHwnLt3Criv7duGlvcsXxCD
n52s13OI6uySXpi05eI3r4EipTKCEJR03P+r9ij70M+mMFeB4YDdMDOveRE0j/4E
s0eRBFRRVuhuvUYbyTusW8lgdnzf63U5OgBb30K/GOHUwR3gwlycbeVOpI7pg3jV
sNdv9rHxQ0n8ohC2GUsrHBxuq83Jk1zeo/9R0ENPqvkReN9n/ldrjbxqDR1EPd/P
AloiZeA/3p3hTqQmaWmwv8nn5tT8SlICbQXgdlLkfBJwQHpsTaflf5oZX2Rafl+9
irFpjDMcCSdgpqbDtYpSiHDgTLaY1cO8P384eL6MXBECgYEA9tfPh36Cn5UnhzYf
MOUSrV7Qu61aFanvKLYq6MEYcIHkXvo59FANM1HTbvhRsyvpmSRZs1F8+hhkTzPh
aziLUGfvpy4MY+KRH1iXnrmySmTAf2Ry3ddmxLVALgSpNR8C+65WygmcRCB2X0Xc
rEbMGflYIet2fMPnndGVo2T0cv0CgYEA2zknU2a/leYxaz7spXEBhcsAtqtlFsHl
o+IybsCHyg8TQMo1pOydgTjEa5uGToTZWwm3hJHmyujQb/Brj/vDxSfAXskbHOIN
NN/P66rfGC25cn6qr459a7RXvhmdsMVisrE4j3sVJBmKBPZSs05drNyYw2INqvQZ
e+WOPGX2nfMCgYByEzQuSvH07ApTe1iY0RR7mLjgMvHR1zHWX7Ge1TYFMJIorn1A
AgrHr8YFn66qHd4bzufBbiRStBkPXUuMsJn5c78WRLqnIpqsoNWZHfpeVQd9GB/Z
k+VDfPwHCFJmYUmQpHYpcp2MAnCSAQhFeYZzbn8jVdzxNdwBXE1KMKqjxQKBgDaI
tjayFbDFbb+/DIFvZjCROmE2q9QIcgbdqywP6veh3mk8pDGdxuSxaXNXYgbAV42l
EikBXodVeRyPk0JjH+U4qUsq/fqmZSClGIUIoazTGxHXXsCDUsHrP/SDTM3/nDjV
iztuI+kyDTqEyDfgo77vtXTNPJctV/WROlveBYZvAoGBANCVDb/9bL+Sknwk8UUN
qqK1s+/HnLDBZZSGD6e3zfUoBsYtN1PkNmhxrLFsSaRzMEEbIgCCt2bs5vl/DWoi
lcQiNhsWRkdUDXpJd0WeqkGK3Gqb4KimoxdGrhhUQ2JmzqanOCuDpmKDDQDGe7Qy
XRWBqNomtTmVA25kchhzSMBQ
-----END PRIVATE KEY-----
"""
