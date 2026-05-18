package main

import (
	"net/http/httptest"
	"testing"
)

func TestQueryBoolPtr(t *testing.T) {
	t.Parallel()

	tests := []struct {
		name     string
		rawQuery string
		wantNil  bool
		want     bool
	}{
		{name: "missing", rawQuery: "", wantNil: true},
		{name: "true", rawQuery: "includeCurrent=true", want: true},
		{name: "false", rawQuery: "includeCurrent=false", want: false},
		{name: "one", rawQuery: "includeCurrent=1", want: true},
		{name: "zero", rawQuery: "includeCurrent=0", want: false},
		{name: "invalid", rawQuery: "includeCurrent=maybe", wantNil: true},
	}

	for _, tc := range tests {
		tc := tc
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()

			req := httptest.NewRequest("GET", "/api/view/program?"+tc.rawQuery, nil)
			got := queryBoolPtr(req, "includeCurrent")
			if tc.wantNil {
				if got != nil {
					t.Fatalf("queryBoolPtr()=%v, want nil", *got)
				}
				return
			}
			if got == nil {
				t.Fatal("queryBoolPtr()=nil, want non-nil")
			}
			if *got != tc.want {
				t.Fatalf("queryBoolPtr()=%v, want %v", *got, tc.want)
			}
		})
	}
}
