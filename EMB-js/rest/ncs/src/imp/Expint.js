const MAXIT = 100;
const EULER = 0.5772156649;
const FPMIN = 1.0e-30;
const EPS = 1.0e-7;

const expint = (n, x) => {
    let i, ii, nm1;
    let a, b, c, d, del, fact, h, psi, ans;

    nm1 = n - 1;

    if (n < 0 || x < 0.0 || (x == 0.0 && (n == 0 || n == 1)))
        throw new Error("error: n < 0 or x < 0");
    else {
        if (n == 0)
            ans = Math.exp(-x) / x;
        else {
            if (x === 0.0)
                ans = 1.0 / nm1;
            else {
                if (x > 1.0) {
                    b = x + n;
                    c = 1.0 / FPMIN;
                    d = 1.0 / b;
                    h = d;

                    for (i = 1; i <= MAXIT; i++) {
                        a = -i * (nm1 + i);
                        b += 2.0;
                        d = 1.0 / (a * d + b);
                        c = b + a / c;
                        del = c * d;
                        h *= del;

                        if (Math.abs(del - 1.0) < EPS) {
                            return h * Math.exp(-x);
                        }
                    }

                    throw new Error("continued fraction failed in expint");
                } else {
                    ans = (nm1 != 0 ? 1.0 / nm1 : -Math.log(x) - EULER);
                    fact = 1.0;

                    for (i = 1; i <= MAXIT; i++) {
                        fact *= -x / i;

                        if (i != nm1)
                            del = -fact / (i - nm1);
                        else {
                            psi = -EULER;

                            for (ii = 1; ii <= nm1; ii++)
                                psi += 1.0 / ii;

                            del = fact * (-Math.log(x) + psi);
                        }

                        ans += del;

                        if (Math.abs(del) < Math.abs(ans) * EPS) {
                            return ans;
                        }
                    }
                    throw new Error("series failed in expint");
                }
            }
        }
    }
    return ans;
};

module.exports = {expint};
