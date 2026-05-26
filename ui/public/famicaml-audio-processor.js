// FamiCaml audio worklet processor.
//
// Main thread pushes Float32Array samples via port.postMessage.
// process() consumes from an internal ring buffer at the audio rate
// (= AudioContext.sampleRate, ~128 samples per call).
//
// Ring buffer is sized for ~250ms latency at 48kHz so that minor jitter
// in the emulator's frame pacing doesn't underflow. Underflow yields silence
// (which is audible but graceful).

const BUFFER_SIZE = 16384; // ~340ms at 48kHz

class FamiCamlProcessor extends AudioWorkletProcessor {
  constructor() {
    super();
    this.buffer = new Float32Array(BUFFER_SIZE);
    this.writeIdx = 0;
    this.readIdx = 0;
    this.lastSample = 0;
    this.port.onmessage = (e) => {
      const samples = e.data;
      // Direct copy into ring buffer. On overflow, drop oldest (advance readIdx).
      for (let i = 0; i < samples.length; i++) {
        this.buffer[this.writeIdx] = samples[i];
        this.writeIdx = (this.writeIdx + 1) % BUFFER_SIZE;
        if (this.writeIdx === this.readIdx) {
          this.readIdx = (this.readIdx + 1) % BUFFER_SIZE;
        }
      }
    };
  }

  process(_inputs, outputs) {
    const out = outputs[0][0];
    // APU mixer output is roughly [0, 0.3]. Center it to [-1, 1]-ish by
    // subtracting a DC offset and scaling. We keep DC removal minimal:
    // just subtract the running mean (lastSample) so as to track slow drift.
    for (let i = 0; i < out.length; i++) {
      if (this.readIdx !== this.writeIdx) {
        const s = this.buffer[this.readIdx];
        this.readIdx = (this.readIdx + 1) % BUFFER_SIZE;
        // Simple high-pass: y = x - 0.999 * x_prev_smoothed
        // Avoid heavy filter, just use a 1-pole IIR for DC removal.
        const filtered = s - this.lastSample;
        this.lastSample = this.lastSample + 0.001 * (s - this.lastSample);
        out[i] = filtered * 2.0; // amplify to a reasonable level
      } else {
        out[i] = 0;
      }
    }
    return true;
  }
}

registerProcessor("famicaml-audio", FamiCamlProcessor);
