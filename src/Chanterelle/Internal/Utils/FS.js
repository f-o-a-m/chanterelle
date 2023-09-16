"use strict";

import fs from "fs";

export const _O_TRUNC = fs.constants ? fs.constants.O_TRUNC : fs.O_TRUNC;
export const _O_CREAT = fs.constants ? fs.constants.O_CREAT : fs.O_CREAT;
export const _O_RDWR = fs.constants ? fs.constants.O_RDWR : fs.O_RWDR;
export const _O_SYNC = fs.constants ? fs.constants.O_SYNC : fs.O_SYNC;
export const readFileSync = fs.readFileSync;
export const writeFileSync = fs.writeFileSync;