🎵 Music Synthesizer & Audio Processor

A complete audio synthesis and processing system implemented in Haskell.
📦 Two Independent Projects
Part 1: Core Synthesizer (music-synth-core)

Implements the synthesizer from Chapters 10-11 of "Learn Haskell by Example":

    🎼 DSL for musical compositions (notes, chords, sequences, tempo)

    📈 Waveform generation (sine, sawtooth, square, triangle)

    🎚️ ADSR envelopes for realistic sound

    🎵 Export to WAV format

    🖥️ Simple command-line interface

Part 2: Advanced Audio Processor (music-synth-pro)

Extends the core with professional audio processing:

    🔍 WAV file parser – handles mono/stereo, validates headers

    📝 Score parser – converts text notation to audio

    ✂️ Audio editing – cut, splice, and rearrange audio segments

    🎛️ 10+ audio effects:

        Gain control & normalization

        Pitch shifting (proportional/absolute)

        Noise gating (threshold-based)

        Echo with configurable delay/decay

        Distortion with saturation control

    🔊 Audio mixing – combine synthesized & external audio

    🖱️ Interactive CLI – user-friendly workflow with error handling
