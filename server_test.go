package main

import (
	"reflect"
	"testing"
)

func Test_scanPoints(t *testing.T) {
	tests := []struct {
		name         string
		arg          []byte
		wantPoints   []point
		wantUnparsed []byte
	}{{name: "normal input",
		arg:          []byte("43,324\r\n324,2123\r\n4234,2342\r\n"),
		wantPoints:   []point{{43.0, 324.0}, {324.0, 2123}, {4234, 2342}},
		wantUnparsed: []byte{}},
		{name: "truncated input at beggining",
			arg:          []byte("324\r\n324,2123\r\n4234,2342\r\n"),
			wantPoints:   []point{{324.0, 2123}, {4234, 2342}},
			wantUnparsed: []byte{}},
		/* {name: "truncated input at end",
		arg:          []byte("43,324\r\n324,2123\r\n4234,23"),
		wantPoints:   []point{{43.0, 324.0}, {324.0, 2123}},
		wantUnparsed: []byte("4234,23")}, */
		{name: "rando error in the midle, invalid utf8",
			arg:          []byte("43,324\r\n324,\xa0\xa12123\r\n4234,2342"),
			wantPoints:   []point{{43.0, 324.0}, {4234, 2342}},
			wantUnparsed: []byte("")},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			gotPoints, gotUnparsed := scanPoints([]point{}, tt.arg)
			if !reflect.DeepEqual(gotPoints, tt.wantPoints) {
				t.Errorf("scanPoints() gotPoints = %v, want %v", gotPoints, tt.wantPoints)
			}
			if !reflect.DeepEqual(gotUnparsed, tt.wantUnparsed) {
				t.Errorf("scanPoints() gotUnparsed = %v, want %v", gotUnparsed, tt.wantUnparsed)
			}
		})
	}
}
