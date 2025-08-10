import numpy as np
import matplotlib.pyplot as plt

# Given data
frequencies = np.array([250, 500, 1000])  # in Hz
Uc = 3  # Voltage in Volts
C = 1e-6  # Capacitance in Farads

# Angular frequencies
omega = 2 * np.pi * frequencies

# Reactance
Xc = 1 / (omega * C)

# Current through the capacitor
Ic = Uc / Xc

# Plotting
plt.figure(figsize=(10, 6))
plt.plot(frequencies, Ic, 'bo-', label='Current through Capacitor')
plt.xlabel('Frequency (Hz)')
plt.ylabel('Current (A)')
plt.title('Current vs Frequency for a Capacitor')
plt.grid(True)
plt.legend()
plt.show()
