package com.OhRyue.certpilot.learn.service;

public class EmaCalculator {
    private final double alpha;
    public EmaCalculator(double alpha) { this.alpha = alpha; }

    public double update(double prev, double cur) {
        return alpha * cur + (1 - alpha) * prev;
    }
}
