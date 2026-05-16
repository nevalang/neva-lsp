package main

import "testing"

func TestStandaloneListenAddr(t *testing.T) {
	t.Parallel()

	cases := []struct {
		name    string
		port    int
		want    string
		wantErr bool
	}{
		{name: "valid", port: 7788, want: "127.0.0.1:7788"},
		{name: "lower bound", port: 1, want: "127.0.0.1:1"},
		{name: "upper bound", port: 65535, want: "127.0.0.1:65535"},
		{name: "too small", port: 0, wantErr: true},
		{name: "too large", port: 65536, wantErr: true},
	}

	for _, tc := range cases {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			got, err := standaloneListenAddr(tc.port)
			if tc.wantErr {
				if err == nil {
					t.Fatalf("standaloneListenAddr(%d) error=nil, want error", tc.port)
				}
				return
			}
			if err != nil {
				t.Fatalf("standaloneListenAddr(%d) unexpected error: %v", tc.port, err)
			}
			if got != tc.want {
				t.Fatalf("standaloneListenAddr(%d)=%q, want %q", tc.port, got, tc.want)
			}
		})
	}
}
