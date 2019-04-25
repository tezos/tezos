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
