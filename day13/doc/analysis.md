# Searching for t

First we find the relative timestamps for each bus:

    ([29 0] [41 19] [577 29] [13 42] [17 43] [19 48] [23 52] [601 60] [37 97])

Bus 29 leaves at t, bus 41 leaves at t + 19, bus 577 leaves at t + 29, etc.

Therefore we have the following system of equations:

    t      = 29  * k1 => t = 29  * k1
    t + 19 = 41  * k2 => t = 41  * k2 - 19
    t + 29 = 577 * k3 => t = 577 * k3 - 29
    t + 42 = 13  * k4 => t = 13  * k4 - 42
    t + 43 = 17  * k4 => t = 17  * k4 - 43
    t + 48 = 19  * k5 => t = 19  * k5 - 48
    t + 52 = 23  * k6 => t = 23  * k6 - 52
    t + 60 = 601 * k7 => t = 601 * k7 - 60
    t + 97 = 37  * k8 => t = 37  * k8 - 97

where k1-k8 are positive integers. We must k1-k8 such that all right hand sides
are equal to t. The minimum t is the solution we are looking for.

In order to solve the problem in an efficient manner, let's take the following equations:

    t = 577 * k3 - 29
    t = 601 * k7 - 60

    => 577 * k3 - 29 = 601 * k7 - 60
    => 601 * k7 - 577 * k3 = 31

This is a solvable diophantine equation because gcd(601, 577) = 1 and 1 divides 31.
To solve it, we first apply the euclidean division algorithm like so:

    601 = 1 * 577 + 24
    => 577 = 24 * 24 + 1
    => 24 = 24 * 1 + 0

Therefore working our way backwards:

    1 = 577 - 24 * 24
    => 1 = 577 - 24 * (601 - 577)
    => 1 = 601 * (-24) - (-25) * 577
    => 31 = 601 * (-31 * 24) - (-31 * 25) * 577
    => 31 = 601 * (-744) - (-775) * 577

So one solution is (k7, k3) = (-744, -775). Other solutions will have the form:

    k7 = -744 - 577 * X
    k3 = -775 - 601 * X

where X is integer. Now, we are interested in the value of t. Let's substitute the last
equation to the equation from the initial system:

    t = 577 * k3 - 29

    k3 = -775 - 601 * X
    => t = 577 * (-775 - 601 * X) - 29
    => t = -447175 - 346777 * X - 29
    => t = -447204 - 346777 * X

This narrows down a lot the values of t that we need to check. To speed up the process even
more, we can skip checking the buses with ids 577 and 601 since the corresponding
equations are already satisfied.
