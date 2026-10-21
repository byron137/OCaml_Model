// CANDIDATE - NOT THE PROJECT FILE. ks re-derived as log2(cdf[s+1]-cdf[s]) from
// codebook_cdf.inc / codebook_km.inc so that encoder ks agrees with codebook_r.inc.
// 40 entries differ from rtl/codebook_ks.inc; see MANIFEST.txt.
// Order (0,1)                               32    31    30    29    28    27    26    25    24    23    22    21    20    19    18    17    16    15    14    13    12    11    10    09    08    07    06    05    04    03    02    01    00
assign w_lookup_table_ks[ 0] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd3, 3'd3, 3'd3, 3'd3, 3'd5, 3'd5, 3'd7};
assign w_lookup_table_ks[ 1] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd5, 3'd5, 3'd5, 3'd5, 3'd5, 3'd5, 3'd5};
assign w_lookup_table_ks[ 2] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd5, 3'd5, 3'd5, 3'd5, 3'd5, 3'd5, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd5};
assign w_lookup_table_ks[ 3] = { 3'd2, 3'd2, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3};

// Order (2,3)                                                                                                                                           16    15    14    13    12    11    10    09    08    07    06    05    04    03    02    01    00
assign w_lookup_table_ks[ 4] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd2, 3'd2, 3'd2, 3'd2, 3'd4, 3'd4, 3'd6};
assign w_lookup_table_ks[ 5] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd1, 3'd4, 3'd4, 3'd4, 3'd4, 3'd4, 3'd4, 3'd4};
assign w_lookup_table_ks[ 6] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd1, 3'd1, 3'd2, 3'd2, 3'd2, 3'd2, 3'd2, 3'd2, 3'd3, 3'd3, 3'd3, 3'd3, 3'd4, 3'd4, 3'd5};
assign w_lookup_table_ks[ 7] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd2, 3'd2, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3, 3'd3};

// Order (4,5)
assign w_lookup_table_ks[ 8] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd2, 3'd2, 3'd4};
assign w_lookup_table_ks[ 9] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd3, 3'd3, 3'd3};
assign w_lookup_table_ks[10] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd2, 3'd2, 3'd2, 3'd3, 3'd3};
assign w_lookup_table_ks[11] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd1, 3'd1, 3'd2, 3'd2, 3'd2, 3'd2, 3'd2, 3'd2, 3'd2};

// Order (6)
assign w_lookup_table_ks[12] = { 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0, 3'd0};