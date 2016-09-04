(in-package :IAE)

(cffi::defcfun ("iae_new" iae_new) :POINTER (SR :INT) (BSIZE :INT) (NCHAN :INT) (NVOICE :INT))
(cffi::defcfun ("iae_delete" iae_delete) :VOID (SELF :POINTER))
(cffi::defcfun ("iae_synth" iae_synth) :INT (SELF :POINTER) (NSAMP :INT) (AUDIOOUT :POINTER) (NCHAN :INT))
(cffi::defcfun ("iae_info_get_string" iae_info_get_string) :STRING (SELF :POINTER) (ARG :STRING))
(cffi::defcfun ("iae_read" iae_read) :INT (SELF :POINTER) (AUDIOFILENAME :STRING) (DESCRIPTIONFILENAME :pointer))
(cffi::defcfun ("iae_read_clip" iae_read_clip) :INT (SELF :POINTER) (NAME :STRING) (NSAMP :INT) (NCHAN :INT) (SAMPLES :POINTER) (SR :DOUBLE))
(cffi::defcfun ("iae_trigger" iae_trigger) :VOID (SELF :POINTER))
(cffi::defcfun ("iae_select_new" iae_select_new) :INT (SELF :POINTER) (FORCETRIGGER :BOOLEAN))
(cffi::defcfun ("iae_clear_selection" iae_clear_selection) :VOID (SELF :POINTER))
(cffi::defcfun ("iae_set_IncludeAll" iae_set_IncludeAll) :BOOLEAN (SELF :POINTER) (FLAG :BOOLEAN))
(cffi::defcfun ("iae_update_kdtree" iae_update_kdtree) :BOOLEAN (SELF :POINTER) (REBUILD :BOOLEAN))

(cffi::defcfun ("iae_conv_minmax_to_descriptor" iae_conv_minmax_to_descriptor) :FLOAT (SELF :POINTER) (DESCRID :INT) (VALNORM :FLOAT))
(cffi::defcfun ("iae_conv_meanstd_to_descriptor" iae_conv_meanstd_to_descriptor) :FLOAT (SELF :POINTER) (DESCRID :INT) (VALMEANSTD :FLOAT))
(cffi::defcfun ("iae_conv_descriptor_to_minmax" iae_conv_descriptor_to_minmax) :FLOAT (SELF :POINTER) (DESCRID :INT) (VALDESCR :FLOAT))
(cffi::defcfun ("iae_conv_descriptor_to_meanstd" iae_conv_descriptor_to_meanstd) :FLOAT (SELF :POINTER) (DESCRID :INT) (VALDESCR :FLOAT))
(cffi::defcfun ("iae_conv_position_to_marker" iae_conv_position_to_marker) :INT (SELF :POINTER) (BUFFER :INT) (POSITION :DOUBLE))
(cffi::defcfun ("iae_conv_marker_to_position" iae_conv_marker_to_position) :DOUBLE (SELF :POINTER) (BUFFER :INT) (MARKER :INT))

;;; PIPO
(cffi::defcfun ("iae_pipo_create" iae_pipo_create) :INT (iae :POINTER) (NAME :STRING))
(cffi::defcfun ("iae_pipo_run" iae_pipo_run) :INT (iae :POINTER) (INDEX :int))
(cffi::defcfun ("iae_pipo_param_num" iae_pipo_param_num) :INT (iae :POINTER))
(cffi::defcfun ("iae_pipo_param_get_name" iae_pipo_param_get_name) :STRING (iae :POINTER) (paramindex :int))
(cffi::defcfun ("iae_pipo_param_get_type" iae_pipo_param_get_type) :int (iae :POINTER) (paramindex :int))
(cffi::defcfun ("iae_pipo_param_get_description" iae_pipo_param_get_description) :string (iae :POINTER) (paramindex :int))
(cffi::defcfun ("iae_pipo_param_enum_get_num" iae_pipo_param_enum_get_num) :int (iae :POINTER) (enumattrname :string))
(cffi::defcfun ("iae_pipo_param_enum_get_element" iae_pipo_param_enum_get_element) :string (iae :POINTER) (enumattrname :string) (elem :int))
(cffi::defcfun ("iae_pipo_param_set_int" iae_pipo_param_set_int) :int (iae :POINTER) (attrname :string) (index :int) (value :int))
(cffi::defcfun ("iae_pipo_param_set_float" iae_pipo_param_set_float) :int (iae :POINTER) (attrname :string) (index :int) (value :double))
(cffi::defcfun ("iae_pipo_param_set_string" iae_pipo_param_set_string) :int (iae :POINTER) (attrname :string) (index :int) (value :string))

(cffi::defcfun ("iae_set_VoiceIndex" iae_set_VoiceIndex) :VOID (SELF :POINTER) (I :INT))
(cffi::defcfun ("iae_set_MarkerTrackSdif" iae_set_MarkerTrackSdif) :VOID (SELF :POINTER) (STREAMID :INT) (FSIG :STRING) (MSIG :STRING))
(cffi::defcfun ("iae_set_DescriptorTrackSdif" iae_set_DescriptorTrackSdif) :VOID (SELF :POINTER) (STREAMID :INT) (FSIG :STRING) (MSIG :STRING))

(cffi::defcfun ("iae_get_NumSources" iae_get_NumSources) :INT (SELF :POINTER))
(cffi::defcfun ("iae_get_AudioDuration" iae_get_AudioDuration) :DOUBLE (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_NumMarkers" iae_get_NumMarkers) :INT (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_NumDescriptors" iae_get_NumDescriptors) :INT (SELF :POINTER))
(cffi::defcfun ("iae_get_DescriptorName" iae_get_DescriptorName) :STRING (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_descriptor_data" iae_get_descriptor_data) :double (SELF :POINTER) (BUFFER :INT) (INDEX :INT) (OUTDATA :POINTER))
(cffi::defcfun ("iae_get_descriptor_stat" iae_get_descriptor_stat) :VOID (SELF :POINTER) (WHICH :INT) (OUTDATA :POINTER))
(cffi::defcfun ("iae_get_NumSelected" iae_get_NumSelected) :INT (SELF :POINTER))
(cffi::defcfun ("iae_get_SelectedSourceIndex" iae_get_SelectedSourceIndex) :INT (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_SelectedSegmentIndex" iae_get_SelectedSegmentIndex) :INT (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_SelectedDistance" iae_get_SelectedDistance) :FLOAT (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_DescriptorTrack" iae_get_DescriptorTrack) :BOOLEAN (SELF :POINTER) (P :POINTER))
(cffi::defcfun ("iae_get_NumData" iae_get_NumData) :INT (SELF :POINTER))
(cffi::defcfun ("iae_get_array_SelectedSourceIndex" iae_get_array_SelectedSourceIndex) :INT (SELF :POINTER) (ARR :POINTER))
(cffi::defcfun ("iae_get_array_SelectedSegmentIndex" iae_get_array_SelectedSegmentIndex) :INT (SELF :POINTER) (ARR :POINTER))
(cffi::defcfun ("iae_get_array_SelectedDistance" iae_get_array_SelectedDistance) :INT (SELF :POINTER) (ARR :POINTER))
(cffi::defcfun ("iae_get_track_size" iae_get_track_size) :int (SELF :POINTER) (buffer :int) (track :int))
(cffi::defcfun ("iae_get_num_params" iae_get_num_params) :INT)
(cffi::defcfun ("iae_get_param_name" iae_get_param_name) :STRING (I :INT))

(cffi::defcfun ("iae_get_array_AudioDuration" iae_get_array_AudioDuration) :INT (SELF :POINTER) (ARR :POINTER))
(cffi::defcfun ("iae_get_array_NumMarkers" iae_get_array_NumMarkers) :INT (SELF :POINTER) (ARR :POINTER))

(cffi::defcfun ("iae_set_Position" iae_set_Position) :VOID (SELF :POINTER) (POSITION :DOUBLE) (TIME :DOUBLE))
(cffi::defcfun ("iae_get_Position" iae_get_Position) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_RandomMode" iae_set_RandomMode) :VOID (SELF :POINTER) (MODE :INT))
(cffi::defcfun ("iae_get_RandomMode" iae_get_RandomMode) :BOOLEAN (SELF :POINTER) (MODE :POINTER))

(cffi::defcfun ("iae_set_ScalingMode" iae_set_ScalingMode) :VOID (SELF :POINTER) (MODE :INT))
(cffi::defcfun ("iae_get_ScalingMode" iae_get_ScalingMode) :BOOLEAN (SELF :POINTER) (MODE :POINTER))

(cffi::defcfun ("iae_set_FilterMode" iae_set_FilterMode) :VOID (SELF :POINTER) (MODE :INT))
(cffi::defcfun ("iae_get_FilterMode" iae_get_FilterMode) :BOOLEAN (SELF :POINTER) (MODE :POINTER))

(cffi::defcfun ("iae_set_SynthMode" iae_set_SynthMode) :VOID (SELF :POINTER) (MODE :INT))
(cffi::defcfun ("iae_get_SynthMode" iae_get_SynthMode) :INT (SELF :POINTER))

(cffi::defcfun ("iae_set_PositionVar" iae_set_PositionVar) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_PositionVar" iae_get_PositionVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Period" iae_set_Period) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_Period" iae_get_Period) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_PeriodVar" iae_set_PeriodVar) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_PeriodVar" iae_get_PeriodVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Duration" iae_set_Duration) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_Duration" iae_get_Duration) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_DurationVar" iae_set_DurationVar) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_DurationVar" iae_get_DurationVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Attack" iae_set_Attack) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_Attack" iae_get_Attack) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Release" iae_set_Release) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE))
(cffi::defcfun ("iae_get_Release" iae_get_Release) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Resampling" iae_set_Resampling) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Resampling" iae_get_Resampling) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_ResamplingVar" iae_set_ResamplingVar) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_ResamplingVar" iae_get_ResamplingVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_FilterFreq" iae_set_FilterFreq) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_FilterFreq" iae_get_FilterFreq) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_FilterFreqVar" iae_set_FilterFreqVar) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_FilterFreqVar" iae_get_FilterFreqVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_FilterQ" iae_set_FilterQ) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_FilterQ" iae_get_FilterQ) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_FilterQVar" iae_set_FilterQVar) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_FilterQVar" iae_get_FilterQVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_FilterGain" iae_set_FilterGain) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_FilterGain" iae_get_FilterGain) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Gain" iae_set_Gain) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Gain" iae_get_Gain) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Level" iae_set_Level) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Level" iae_get_Level) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_LevelVar" iae_set_LevelVar) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_LevelVar" iae_get_LevelVar) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_SourceIndex" iae_set_SourceIndex) :VOID (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_SourceIndex" iae_get_SourceIndex) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_MarkerIndex" iae_set_MarkerIndex) :VOID (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_MarkerIndex" iae_get_MarkerIndex) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_RepeatMarkers" iae_set_RepeatMarkers) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_RepeatMarkers" iae_get_RepeatMarkers) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Cyclic" iae_set_Cyclic) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_Cyclic" iae_get_Cyclic) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_MicroTiming" iae_set_MicroTiming) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_MicroTiming" iae_get_MicroTiming) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_CenteredGrains" iae_set_CenteredGrains) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_CenteredGrains" iae_get_CenteredGrains) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Advance" iae_set_Advance) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Advance" iae_get_Advance) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Offset" iae_set_Offset) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Offset" iae_get_Offset) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Delay" iae_set_Delay) :VOID (SELF :POINTER) (P :DOUBLE))
(cffi::defcfun ("iae_get_Delay" iae_get_Delay) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_DuplicateChannels" iae_set_DuplicateChannels) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_DuplicateChannels" iae_get_DuplicateChannels) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_OutputChannelBalance" iae_set_OutputChannelBalance) :VOID (SELF :POINTER) (P1 :DOUBLE) (P2 :DOUBLE) (P3 :DOUBLE))
(cffi::defcfun ("iae_get_OutputChannelBalance" iae_get_OutputChannelBalance) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Radius" iae_set_Radius) :VOID (SELF :POINTER) (P :FLOAT))
(cffi::defcfun ("iae_get_Radius" iae_get_Radius) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_K" iae_set_K) :VOID (SELF :POINTER) (P :INT))
(cffi::defcfun ("iae_get_K" iae_get_K) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Target" iae_set_Target) :INT (SELF :POINTER) (LEN :INT) (P :POINTER))
(cffi::defcfun ("iae_get_Target" iae_get_Target) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Weight" iae_set_Weight) :INT (SELF :POINTER) (LEN :INT) (P :POINTER))
(cffi::defcfun ("iae_get_Weight" iae_get_Weight) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_IncludeSource" iae_set_IncludeSource) :INT (SELF :POINTER) (LEN :INT) (P :POINTER))
(cffi::defcfun ("iae_get_IncludeSource" iae_get_IncludeSource) :BOOLEAN (SELF :POINTER) (P :POINTER))

(cffi::defcfun ("iae_set_Play" iae_set_Play) :VOID (SELF :POINTER) (P :BOOLEAN))
(cffi::defcfun ("iae_get_Play" iae_get_Play) :BOOLEAN (SELF :POINTER) (P :POINTER))




